module IO = struct
  type 'a t = 'a Conduit_lwt_unix.io
  let return = Lwt.return
  let ( >>= ) = Lwt.bind
  let catch = Lwt.catch
end

module Reader = Resp.Reader (struct
  module IO: Resp.IO with type 'a t = 'a Lwt.t = IO
  type ic = Lwt_io.input_channel
  let read ic n = Lwt_io.read ic ~count:n
  let read_char = Lwt_io.read_char
  let read_line = Lwt_io.read_line
end)

module Writer = Resp.Writer (struct
  module IO: Resp.IO with type 'a t = 'a Lwt.t = IO

  type oc = Lwt_io.output_channel

  let write oc s =
    let open IO in
    Lwt_io.write oc s >>= fun () ->
    Lwt_io.flush oc
end)

module Backend(Data: sig type data end) = struct
  include Data

  module IO = IO

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  type server = Conduit_lwt_unix.ctx * Conduit_lwt_unix.server
  type client = ic * oc

  let ic (ic, _) = ic
  let oc (_, oc) = oc

  let run server fn =
    let mode = snd server in
    let ctx = fst server in
    Conduit_lwt_unix.serve ~ctx ~mode (fun _ ic oc -> fn (ic, oc))
end

module Client_backend = struct
  open Lwt.Infix
  module IO = IO

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type params = Conduit_lwt_unix.ctx * Conduit_lwt_unix.client

  let connect params =
    Conduit_lwt_unix.connect ~ctx:(fst params) (snd params) >|= fun (_, ic, oc) -> (ic, oc)
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
