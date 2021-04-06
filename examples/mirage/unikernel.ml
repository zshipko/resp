open Lwt.Infix
open Mirage_types_lwt
module Server = Resp_mirage.Server.Default

let commands =
  [
    ( "ping",
      fun _ client cmd nargs ->
        if nargs = 0 then Server.ok client
        else Server.recv client >>= fun x -> Server.send client x );
  ]

module Main (Clock : PCLOCK) (Conduit : Conduit_mirage.S) = struct
  let start clock conduit =
    let port = 8888 in
    let addr = Ipaddr.of_string_exn "0.0.0.0" in
    let endp = `TCP (addr, port) in
    Conduit_mirage.server endp >>= fun server ->
    Logs.info (fun f -> f "Starting server on port 8888");
    let server = Server.create ~commands (conduit, server) () in
    Server.start server
end
