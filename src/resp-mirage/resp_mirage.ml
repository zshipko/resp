open Lwt.Infix

module Make (C : Conduit_mirage.S) = struct
  type buffer = { flow : C.Flow.flow; mutable buffer : Cstruct.t }

  module Reader = Resp.Reader (struct
    type ic = buffer

    let update_buffer_if_needed t n =
      let buflen = Cstruct.length t.buffer in
      if buflen < n then
        C.Flow.read t.flow >|= function
        | Ok (`Data c) -> t.buffer <- Cstruct.append t.buffer c
        | Ok `Eof -> raise End_of_file
        | Error err ->
            let msg = Fmt.to_to_string C.Flow.pp_error err in
            raise (Resp.Exc (`Msg msg))
      else Lwt.return_unit

    let get_string t n =
      let buflen = Cstruct.length t.buffer in
      if n > buflen then raise End_of_file
      else
        let c, d = Cstruct.split t.buffer n in
        t.buffer <- d;
        Cstruct.to_string c

    let read t n = update_buffer_if_needed t n >|= fun () -> get_string t n

    let read_char t =
      let rec aux () =
        let buflen = Cstruct.length t.buffer in
        if buflen > 0 then
          let c = Cstruct.get_char t.buffer 0 in
          let () = t.buffer <- Cstruct.sub t.buffer 1 (buflen - 1) in
          Lwt.return c
        else
          C.Flow.read t.flow >>= function
          | Ok (`Data c) ->
              t.buffer <- Cstruct.append t.buffer c;
              aux ()
          | Ok `Eof -> raise End_of_file
          | Error err ->
              let msg = Fmt.to_to_string C.Flow.pp_error err in
              raise (Resp.Exc (`Msg msg))
      in
      aux ()

    let read_line t =
      let rec aux output =
        read t 1 >>= function
        | "\n" -> Lwt.return output
        | "\r" -> aux output
        | c -> aux (output ^ c)
      in
      aux ""
  end)

  module Writer = Resp.Writer (struct
    type oc = C.Flow.flow

    let write oc s =
      C.Flow.write oc (Cstruct.of_string s) >>= function
      | Ok () -> Lwt.return_unit
      | Error err ->
          let msg = Fmt.to_to_string C.Flow.pp_write_error err in
          raise (Resp.Exc (`Msg msg))
  end)

  module Backend (Data : Resp_server.DATA) = struct
    include Data

    type ic = buffer

    type oc = C.Flow.flow

    type server = C.t * Conduit_mirage.server

    let run (ctx, server) fn =
      C.listen ctx server (fun flow ->
          let buffer = { flow; buffer = Cstruct.empty } in
          fn (buffer, flow))
  end

  module Client_backend = struct
    open Lwt.Infix

    type ic = buffer

    type oc = C.Flow.flow

    type params = C.t * Conduit_mirage.client

    let connect (ctx, client) =
      C.connect ctx client >|= fun c -> ({ flow = c; buffer = Cstruct.empty }, c)
  end

  module Server = struct
    module Make (Auth : Resp_server.AUTH) (Data : Resp_server.DATA) =
      Resp_server.Make (Backend (Data)) (Auth) (Resp.Make (Reader) (Writer))

    module Default =
      Make
        (Resp_server.Auth.String)
        (struct
          type data = unit

          module Client = struct
            type t = unit

            let init _ = ()
          end
        end)
  end

  module Client =
    Resp_client.Make (Client_backend) (Resp.Make (Reader) (Writer))
end
