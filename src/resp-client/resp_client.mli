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
    (S : Resp.S with type Reader.ic = Client.ic and type Writer.oc = Client.oc) :
  S
  with module Reader = S.Reader
   and module Writer = S.Writer
   and type ic = Client.ic
   and type oc = Client.oc
   and type params = Client.params
