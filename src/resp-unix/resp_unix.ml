module IO = Resp.IO.Default

module Reader = Resp.Reader(struct
  module IO = IO
  type ic = in_channel

  let read_char ic = input_char ic
  let read_line ic = input_line ic
  let read ic n = really_input_string ic n
end)

module Writer = Resp.Writer(struct
  module IO = IO
  type oc = out_channel

  let write = output_string
end)

module Backend(Data: sig type data end) = struct
  module IO = IO

  type ic = in_channel
  type oc = out_channel

  type client = ic * oc
  type data = Data.data
  type server = Unix.sockaddr

  let ic (ic, _) = ic
  let oc (_, oc) = oc

  let run server fn =
    Unix.establish_server (fun ic oc -> fn (ic, oc)) server
end

module Client_backend = struct
  module IO = IO

  type ic = in_channel
  type oc = out_channel
  type params = Unix.sockaddr

  let connect params =
    Unix.open_connection params
end

module Bulk_string = Resp.Bulk.String(Reader)(Writer)

module Bulk = struct
  module String = Resp.Make(Bulk_string)
end

module Server = struct
  module Make (Auth: Resp_server.AUTH)(Data: sig type data end)(S: Resp.S with module IO = IO and type Reader.ic = Reader.ic and type Writer.oc = Writer.oc) = Resp_server.Make(Backend(Data))(Auth)(S)
  module Default = Make(Resp_server.Auth.String)(struct type data = unit end)(Bulk.String)
end

module Client(S: Resp.S with module IO = IO and module Reader = Reader and module Writer = Writer) = Resp_client.Make(Client_backend)(S)
