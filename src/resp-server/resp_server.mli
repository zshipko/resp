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

module Auth: sig
  module String: AUTH with type t = string
  module User: AUTH with type t = (string, string) Hashtbl.t
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
    (Value: Resp.S with module IO = Server.IO and type Reader.ic = Server.ic and type Writer.oc = Server.oc):
    S
      with type server = Server.server
      and type client = Server.client
      and module Auth = Auth
      and module IO = Server.IO
      and type ic = Server.ic
      and type oc = Server.oc
      and module Value = Value
      and type data = Server.data

