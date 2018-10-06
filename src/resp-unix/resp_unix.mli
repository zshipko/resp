module IO: Resp.IO with type 'a t = 'a

module Reader: Resp.READER
  with type ic = in_channel
  and module IO = IO

module Writer: Resp.WRITER
  with type oc = out_channel
  and module IO = IO

module Backend(Data: sig type data end): Resp_server.SERVER
  with module IO = IO
  and type oc = out_channel
  and type ic = in_channel

module Bulk: sig
  module String: Resp.S with module IO = IO and type bulk = string and module Reader = Reader and module Writer = Writer
end

module Server: sig
  module Make (Auth: Resp_server.AUTH)(Data: sig type data end)(S: Resp.S with module IO = IO and type Reader.ic = Reader.ic and type Writer.oc = Writer.oc): Resp_server.S
    with module IO = IO
    with module Auth = Auth
    and type ic = Reader.ic
    and type oc = Writer.oc
    and type server = Unix.sockaddr
    and type client = Reader.ic * Writer.oc
    and type data = Data.data
    and module Value = S

  module Default : Resp_server.S
    with module IO = IO
    with type Auth.t = string
    and type ic = Reader.ic
    and type oc = Writer.oc
    and type server = Unix.sockaddr
    and type client = Reader.ic * Writer.oc
    and type data = unit
    and module Value = Bulk.String
end

module Client(S: Resp.S with module IO = IO and module Reader = Reader and module Writer = Writer): Resp_client.S
  with module IO = IO
  and type ic = Reader.ic
  and type oc = Writer.oc
  and type params = Unix.sockaddr
  and type bulk = S.bulk
