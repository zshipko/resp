(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)
open Resp_unix
open IO

module Server = Server.Make
                  (Resp_server.Auth.String)
                  (struct type data = (string, string) Hashtbl.t end)
                  (Bulk.String)

module Client = Client(Bulk.String)

include Util.Make(Server)

let client, pid =
  match Unix.fork () with
  | -1 -> Printf.eprintf "Unable to fork process"; exit 1
  | 0 ->
      let server = Unix.inet_addr_of_string "127.0.0.1" in
      let server = Unix.ADDR_INET (server, 1235) in
      let data = Hashtbl.create 8 in
      let server = Server.create ~commands server data in
      Server.start server >>= fun () ->
      exit 0
  | pid ->
      Unix.sleep 1;
      let client = Unix.inet_addr_of_string "127.0.0.1" in
      let client = Unix.ADDR_INET (client, 1235) in
      Client.connect client, pid

let invalid_response () = Alcotest.fail "Invalid response type"

let test_set () =
  match Client.run_s client [| "set"; "abc"; "123" |] with
  | `String s -> Alcotest.(check string) "set OK" s "OK"
  | _ -> invalid_response ()

let test_get () =
  match Client.decode client @@ Client.run_s client [| "get"; "abc" |] with
  | `Bulk (`String s) -> Alcotest.(check string) "Value of abc" s "123"
  | _ -> invalid_response ()

let test_end () =
  match Client.run_s client [| "end" |] with
  | `String s -> Alcotest.(check string) "end" s "OK"
  | _ -> invalid_response ()


let basic = [
  "Set", `Quick, test_set;
  "Get", `Quick, test_get;
]

let () =
  Alcotest.run ~and_exit:false "Resp_unix" [
    "basic", basic
  ];
  close_in_noerr (fst client);
  Unix.kill pid Sys.sigint

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
