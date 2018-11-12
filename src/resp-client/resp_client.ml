module type S = sig
  include Resp.S

  type ic
  type oc
  type params

  val connect : params -> (ic * oc) IO.t
  val read : ic * oc -> bulk Resp.t IO.t
  val read_s : ic * oc -> bulk Resp.t IO.t
  val write : ic * oc -> bulk Resp.t -> unit IO.t
  val write_s : ic * oc -> bulk Resp.t -> unit IO.t
  val run_s : ic * oc -> string array -> Resp.lexeme IO.t
  val run : ic * oc -> bulk Resp.t array -> Resp.lexeme IO.t
  val decode : ic * oc -> Resp.lexeme -> bulk Resp.t IO.t
  val decode_s : ic * oc -> Resp.lexeme -> bulk Resp.t IO.t
  val read_lexeme : ic * oc -> Resp.lexeme IO.t
end

module type CLIENT = sig
  module IO : Resp.IO

  type ic
  type oc
  type params

  val connect : params -> (ic * oc) IO.t
end

module Make
    (Client : CLIENT)
    (S : Resp.S
         with module IO = Client.IO
          and type Reader.ic = Client.ic
          and type Writer.oc = Client.oc) =
struct
  include S
  open IO

  type ic = Client.ic
  type oc = Client.oc
  type params = Client.params

  let connect = Client.connect
  let read (ic, _) = S.read ic
  let read_s (ic, _) = S.read_s ic
  let write (_, oc) = S.write oc
  let write_s (_, oc) = S.write_s oc
  let decode (ic, _) = S.Reader.decode ?f:decoder ic
  let decode_s (ic, _) = S.Reader.decode ?f:None ic

  let read_lexeme (ic, _) =
    S.Reader.read_lexeme ic >>= fun x -> Resp.unwrap x |> IO.return

  let run_s client cmd =
    let cmd = Array.map (fun s -> `Bulk (`String s)) cmd in
    write client (`Array cmd) >>= fun () -> read_lexeme client

  let run client cmd =
    write client (`Array cmd) >>= fun () -> read_lexeme client
end
