module type AUTH = sig
  type t
  val check: t -> string array -> bool
end

module type SERVER = sig
  module IO: Resp.IO
  type ic
  type oc
  type server
  type data
  type client
  val ic: client -> ic
  val oc: client -> oc
  val run: server -> (client -> unit IO.t) -> unit IO.t
end

module Auth = struct
  module String = struct
    type t = string
    let check auth args =
      Array.length args > 0 && args.(0) = auth
  end

  module User = struct
    type t = (string, string) Hashtbl.t

    let check auth args =
      if Array.length args < 2 then false
      else
        match Hashtbl.find_opt auth args.(0) with
        | Some p -> p = args.(1)
        | None -> false
  end
end

module type S = sig
  include SERVER
  module Value: Resp.S with module IO = IO and type Reader.ic = ic and type Writer.oc = oc
  module Auth: AUTH

  type command =
    data ->
    client ->
    string ->
    int ->
    Value.bulk Resp.t option IO.t

  type t
  val ok: unit -> Value.bulk Resp.t option IO.t
  val error: string -> Value.bulk Resp.t option IO.t
  val invalid_arguments: unit -> Value.bulk Resp.t option IO.t
  val create: ?auth:Auth.t -> ?commands:(string * command) list -> ?default:string -> server -> data -> t
  val start: t -> unit IO.t
end


module Make
    (Server: SERVER)
    (Auth: AUTH)
    (Value: Resp.S with module IO = Server.IO and type Reader.ic = Server.ic and type Writer.oc = Server.oc)
= struct
  include Server
  module Value = Value
  module Auth = Auth

  let ( >>= ) = IO.( >>= )

  type command =
    data ->
    client ->
    string ->
    int ->
    Value.bulk Resp.t option IO.t

  type t = {
    server: server;
    data: data;
    auth: Auth.t option;
    commands: (string, command) Hashtbl.t;
    default:  string;
  }

  let ok () = IO.return (Some (`String "OK"))
  let error msg = IO.return (Some (`Error (Printf.sprintf "ERR %s" msg)))
  let invalid_arguments () = error "Invalid arguments"

  let hashtbl_of_list l =
    let ht = Hashtbl.create (List.length l) in
    List.iter (fun (k, v) ->
      Hashtbl.replace ht k v) l;
    ht
  let create ?auth ?(commands = []) ?(default = "default") server data =
    let commands = hashtbl_of_list commands in
    {server; data; auth; commands; default}

  let check_auth auth args =
    match auth with
    | Some auth -> Auth.check auth args
    | None -> true

  let to_string = function
    | `Bulk (`String s) -> s
    | `String s -> s
    | _ -> raise (Resp.Exc (`Msg "Invalid argument"))

  let split_command_s arr: string * string array =
    String.lowercase_ascii @@ to_string arr.(0),
    Array.map to_string (Array.sub arr 1 (Array.length arr - 1))

  let rec discard_n client n =
    if n > 0 then
      Value.read (ic client) >>= fun _ ->
      discard_n client (n - 1)
    else
      IO.return ()

  let rec handle t client authenticated =
    let argc = ref 0 in
    IO.catch (fun () ->
      if not authenticated then
        handle_not_authenticated t client
      else
      Value.Reader.next (ic client) >>= function
        | Ok (`As n) ->
            argc := n - 1;
            (Value.read_s (ic client) >>= function
              | `String s | `Bulk (`String s) ->
                  let f = try Hashtbl.find t.commands s with Not_found -> Hashtbl.find t.commands t.default in
                  f t.data client s !argc >>= (function
                    | Some x -> Value.write (oc client) x >>= fun () -> handle t client true
                    | None -> IO.return ())
              | _ ->
                  discard_n client !argc >>= fun () ->
                  Value.write (oc client) (`Error "ERR Invalid command") >>= fun () ->
                  handle t client true)
        | _ ->
            Value.write (oc client) (`Error "ERR Invalid command") >>= fun () ->
            handle t client true)
    (function
      | Resp.Exc exc ->
          Value.write (oc client) (`Error ("ERR " ^ Resp.string_of_error exc)) >>= fun () ->
          handle t client true
      | Not_found ->
          discard_n client !argc >>= fun () ->
          Value.write (oc client) (`Error "ERR Command not found") >>= fun () ->
          handle t client true
      | exc -> raise exc)

  and handle_not_authenticated t client =
    Value.read_s (ic client) >>= function
    | `Array arr ->
      let cmd, args = split_command_s arr in
      (match cmd, args with
      | "auth", args ->
          if check_auth t.auth args then
            Value.write (oc client) (`String "OK") >>= fun () -> handle t client true
          else
            Value.write (oc client) (`Error "ERR Authentication required") >>= fun () -> handle t client false
      | _, _ -> Value.write (oc client) (`Error "ERR Authentication required") >>= fun () -> handle t client false)
    | _ -> Value.write (oc client) (`Error "ERR Authentication required") >>= fun () -> handle t client false

  let start t =
    run t.server (fun client ->
      handle t client (t.auth = None)
    )
end
