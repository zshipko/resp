module type S = sig
  include Resp.S

  type ic
  type oc
  type params

  val connect : params -> (ic * oc) Lwt.t
  val read : ic * oc -> Resp.t Lwt.t
  val write : ic * oc -> Resp.t -> unit Lwt.t
  val run : ic * oc -> Resp.t array -> Resp.t Lwt.t
  val run_s : ic * oc -> string array -> Resp.t Lwt.t
  val decode : ic * oc -> Resp.lexeme -> Resp.t Lwt.t
  val read_lexeme : ic * oc -> Resp.lexeme Lwt.t
end

module type CLIENT = sig
  type ic
  type oc
  type params

  val connect : params -> (ic * oc) Lwt.t
end

module Make
    (Client : CLIENT)
    (S : Resp.S with type Reader.ic = Client.ic and type Writer.oc = Client.oc) =
struct
  include S
  open Lwt

  type ic = Client.ic
  type oc = Client.oc
  type params = Client.params

  let connect = Client.connect
  let read (ic, _) = S.read ic
  let write (_, oc) = S.write oc
  let decode (ic, _) = S.Reader.decode ic

  let read_lexeme (ic, _) =
    S.Reader.read_lexeme ic >>= fun x -> Resp.unwrap x |> Lwt.return

  let run_s client cmd =
    let cmd = Array.map (fun s -> `Bulk s) cmd in
    write client (`Array cmd) >>= fun () -> read client

  let run client cmd = write client (`Array cmd) >>= fun () -> read client
end
