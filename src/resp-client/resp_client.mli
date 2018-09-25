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
    (S: Resp.S with module IO = Client.IO and type Reader.ic = Client.ic and type Writer.oc = Client.oc): S
      with module IO = Client.IO
      and type ic = Client.ic
      and type oc = Client.oc
      and type params = Client.params
