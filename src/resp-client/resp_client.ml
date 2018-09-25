module type S = sig
  include Resp.S

  type ic
  type oc
  type params
  val connect: params -> (ic * oc) IO.t
  val read: ic -> bulk Resp.t IO.t
  val write: oc -> bulk Resp.t -> unit IO.t
  val run_s: ic * oc -> string array -> Resp.lexeme IO.t
  val run: ic * oc -> bulk Resp.t array -> Resp.lexeme IO.t
end

module type CLIENT = sig
  module IO: Resp.IO

  type ic
  type oc
  type params
  val connect: params -> (ic * oc) IO.t
end

module Make
    (Client: CLIENT)
    (S: Resp.S with module IO = Client.IO and type Reader.ic = Client.ic and type Writer.oc = Client.oc)
= struct
  include S
  open IO

  type ic = Client.ic
  type oc = Client.oc
  type params = Client.params
  let connect = Client.connect

  let read ic = S.read ic
  let write oc = S.write oc

  let run_s (ic, oc) cmd =
    let cmd = Array.map (fun s -> `Bulk (`String s)) cmd in
    write oc (`Array cmd) >>= fun () ->
    S.Reader.next ic >>= fun x -> Resp.unwrap x |> IO.return

  let run (ic, oc) cmd =
    write oc (`Array cmd) >>= fun () ->
    S.Reader.next ic >>= fun x -> Resp.unwrap x |> IO.return
end
