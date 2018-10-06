(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)
open Lwt.Infix

module Value = Resp.String

let check_value_rountrip =
  let v = `Array [| `String "x"; `Integer 123L; `Bulk (`String "abc") |] in
  let output = ref "" in
  Value.write output v;
  let v' = Value.read output in
  assert (v = v')

open Resp_lwt_unix

module Server = Server.Make
                  (Resp_server.Auth.String)
                  (struct type data = (string, string) Hashtbl.t end)
                  (Bulk.String)

module Client = Client(Resp_lwt_unix.Bulk.String)

let commands = [
  "set", (fun ht client _cmd nargs ->
    if nargs <> 2 then
      Server.invalid_arguments client
    else
      Server.recv client >>= fun key ->
      Server.recv client >>= fun value ->
      Hashtbl.replace ht (Resp.to_string_exn key) (Resp.to_string_exn value);
      Server.ok client
  );
  "get", (fun ht client _cmd nargs ->
    if nargs <> 1 then
      Server.invalid_arguments client
    else
      Server.recv client >>= fun key ->
      try
        let value = Hashtbl.find ht (Resp.to_string_exn key) in
        Server.send client (`Bulk (`String value))
      with Not_found ->
        Server.error client "Not found"
  );
]

let main =
  match Lwt_unix.fork () with
  | -1 -> Printf.eprintf "Unable to fork process"; exit 1
  | 0 ->
      Conduit_lwt_unix.init ~src:"127.0.0.1" () >>= fun ctx ->
      let server = `TCP (`Port 1234) in
      let data = Hashtbl.create 8 in
      let server = Server.create ~commands (ctx, server) data in
      Server.start server
  | n ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      Conduit_lwt_unix.init () >>= fun ctx ->
      let addr = Ipaddr.of_string_exn "127.0.0.1" in
      let params = (ctx, `TCP (`IP addr, `Port 1234)) in
      Client.connect params >>= fun (ic, oc) ->
      Client.run_s (ic, oc) [| "set"; "abc"; "123" |] >>= (function
      | `String s -> print_endline s; if s = "OK" then Lwt.return 0 else Lwt.return 1
      | _ -> Lwt.return 1) >>= fun _code ->
      Client.run_s (ic, oc) [| "BAD"; "x" |] >>= (function
      | `Error e -> print_endline e; Lwt.return 0
      | _ -> Lwt.return 1) >>= fun _code ->
        Client.run_s (ic, oc) [| "get"; "abc" |] >>= Client.decode ic >>= (function
      | `Bulk (`String s) ->
          print_endline s; if s = "123" then Lwt.return 0 else Lwt.return 1
      | `Error e -> print_endline e; Lwt.return 2
      | _ -> Lwt.return 1) >>= fun code ->
      Unix.kill n Sys.sigint;
      exit code

let () = Lwt_main.run main

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
