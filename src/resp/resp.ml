(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type 'a bulk =
  [ `String of string
  | `Value of 'a ]

type 'a t =
  [ `Nil
  | `Integer of int64
  | `String of string
  | `Error of string
  | `Bulk of 'a bulk
  | `Array of 'a t array ]

type lexeme =
  [ `Nil
  | `Integer of int64
  | `String of string
  | `Error of string
  | `Bs of int
  | `As of int ]

type error =
  [ `Msg of string
  | `Unexpected of char
  | `Invalid_value
  | `Invalid_encoder ]

let pp_error fmt = function
  | `Msg s ->
    Format.fprintf fmt "%s" s
  | `Invalid_value ->
    Format.fprintf fmt "invalid value"
  | `Unexpected c ->
    Format.fprintf fmt "unexpected input: (%d)" (int_of_char c)
  | `Invalid_encoder ->
    Format.fprintf fmt "invalid encoder"

let string_of_error x =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  pp_error fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

exception Exc of error

let unwrap = function
  | Ok x ->
    x
  | Error e ->
    raise (Exc e)

module type IO = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type INPUT = sig
  module IO : IO

  type ic

  val read : ic -> int -> string IO.t
  val read_line : ic -> string IO.t
  val read_char : ic -> char IO.t
end

module type OUTPUT = sig
  module IO : IO

  type oc

  val write : oc -> string -> unit IO.t
end

module type READER = sig
  include INPUT

  val discard_sep : ic -> unit IO.t
  val read_lexeme : ic -> (lexeme, error) result IO.t
  val decode : ?f:(ic -> int -> 'a IO.t) -> ic -> lexeme -> 'a t IO.t
end

module type WRITER = sig
  include OUTPUT

  val write_sep : oc -> unit IO.t
  val write_lexeme : oc -> lexeme -> unit IO.t

  val encode :
    ?f:(oc -> 'a -> int * (unit -> unit IO.t)) -> oc -> 'a t -> unit IO.t
end

module type BULK = sig
  module IO : IO
  module Reader : READER with module IO = IO
  module Writer : WRITER with module IO = IO

  type bulk

  val encoder : (Writer.oc -> bulk -> int * (unit -> unit IO.t)) option
  val decoder : (Reader.ic -> int -> bulk IO.t) option
end

module type S = sig
  include BULK

  val write : Writer.oc -> bulk t -> unit IO.t
  val write_s : Writer.oc -> bulk t -> unit IO.t
  val read : Reader.ic -> bulk t IO.t
  val read_s : Reader.ic -> bulk t IO.t
end

module Reader (I : INPUT) = struct
  include I

  let ( >>= ) = IO.( >>= )

  let discard_sep ic =
    I.read_char ic >>= fun _ -> I.read_char ic >>= fun _ -> IO.return ()

  let read_lexeme ic : (lexeme, error) result IO.t =
    I.read_char ic
    >>= function
    | ':' ->
      I.read_line ic
      >>= fun i ->
      let i = Int64.of_string i in
      IO.return @@ Ok (`Integer i)
    | '-' ->
      I.read_line ic >>= fun line -> IO.return @@ Ok (`Error line)
    | '+' ->
      I.read_line ic >>= fun line -> IO.return @@ Ok (`String line)
    | '*' ->
      I.read_line ic
      >>= fun i ->
      let i = int_of_string i in
      if i < 0 then IO.return @@ Ok `Nil else IO.return @@ Ok (`As i)
    | '$' ->
      I.read_line ic
      >>= fun i ->
      let i = int_of_string i in
      if i < 0 then IO.return @@ Ok `Nil else IO.return @@ Ok (`Bs i)
    | c ->
      Printf.printf "Bad char: (%d) %c\n" (int_of_char c) c;
      IO.return @@ Error (`Unexpected c)

  let rec decode ?f ic : lexeme -> 'a t IO.t =
    let f' =
      match f with
      | Some f ->
        fun ic len -> f ic len >>= fun x -> IO.return @@ `Value x
      | None ->
        fun ic len -> read ic len >>= fun x -> IO.return @@ `String x
    in
    function
    | `Nil ->
      IO.return `Nil
    | `Integer i ->
      IO.return @@ `Integer i
    | `Error e ->
      IO.return @@ `Error e
    | `String s ->
      IO.return @@ `String s
    | `Bs len ->
      f' ic len >>= fun b -> discard_sep ic >>= fun () -> IO.return @@ `Bulk b
    | `As len ->
      let arr = Array.make len `Nil in
      let rec aux = function
        | 0 ->
          IO.return ()
        | n -> (
          read_lexeme ic
          >>= function
          | Ok v ->
            decode ?f ic v
            >>= fun x ->
            arr.(len - n) <- x;
            aux (n - 1)
          | Error err ->
            raise (Exc err) )
      in
      aux len >>= fun () -> IO.return @@ `Array arr
end

module Writer (O : OUTPUT) = struct
  include O

  let ( >>= ) = IO.( >>= )
  let write_sep oc = O.write oc "\r\n"

  let write_lexeme oc = function
    | `Nil ->
      O.write oc "*-1\r\n"
    | `Error e ->
      O.write oc "-" >>= fun () -> O.write oc e >>= fun () -> write_sep oc
    | `Integer i ->
      O.write oc ":" >>= fun () -> O.write oc (Printf.sprintf "%Ld\r\n" i)
    | `Bs len ->
      O.write oc (Printf.sprintf "$%d\r\n" len)
    | `As len ->
      O.write oc (Printf.sprintf "*%d\r\n" len)
    | `String s ->
      O.write oc "+" >>= fun () -> O.write oc s >>= fun () -> write_sep oc

  let rec encode ?f oc = function
    | `Nil ->
      write_lexeme oc `Nil
    | `Error e ->
      write_lexeme oc (`Error e)
    | `String s ->
      write_lexeme oc (`String s)
    | `Integer i ->
      write_lexeme oc (`Integer i)
    | `Bulk (`Value b) -> (
      match f with
      | Some f ->
        let i, f = f oc b in
        write_lexeme oc (`Bs i) >>= fun () -> f () >>= fun () -> write_sep oc
      | None ->
        raise (Exc `Invalid_encoder) )
    | `Bulk (`String s) ->
      let len = String.length s in
      write_lexeme oc (`Bs len)
      >>= fun () -> write oc s >>= fun () -> write_sep oc
    | `Array a ->
      let len = Array.length a in
      let rec write i =
        match i with
        | 0 ->
          IO.return ()
        | n ->
          encode ?f oc a.(len - i) >>= fun () -> write (n - 1)
      in
      write_lexeme oc (`As len) >>= fun () -> write len
end

module Make (Bulk : BULK) = struct
  include Bulk

  let ( >>= ) = IO.( >>= )
  let decode = Reader.decode ?f:Bulk.decoder
  let decode_s ic = Reader.decode ?f:None ic

  let read ic =
    Reader.read_lexeme ic
    >>= function
    | Ok l ->
      decode ic l
    | Error e ->
      raise (Exc e)

  let read_s ic =
    Reader.read_lexeme ic
    >>= function
    | Ok l ->
      decode_s ic l
    | Error e ->
      raise (Exc e)

  let encode = Writer.encode ?f:Bulk.encoder
  let encode_s oc = Writer.encode ?f:None oc
  let write oc = encode oc
  let write_s oc x = encode_s oc x
end

module Bulk = struct
  module Json (Reader : READER) (Writer : WRITER with module IO = Reader.IO) =
  struct
    module IO = Reader.IO
    module Reader = Reader
    module Writer = Writer

    type bulk = Ezjsonm.t

    (* TODO: make encoder/decoder streaming *)
    let encoder =
      Some
        (fun oc bulk ->
          let s = Ezjsonm.to_string bulk in
          (String.length s, fun () -> Writer.write oc s) )

    let decoder =
      Some
        (fun ic len ->
          let open IO in
          Reader.read ic len >>= fun s -> Ezjsonm.from_string s |> return )
  end

  module String (Reader : READER) (Writer : WRITER with module IO = Reader.IO) =
  struct
    module IO = Reader.IO
    module Reader = Reader
    module Writer = Writer

    type bulk = string

    let encoder =
      Some (fun oc bulk -> (String.length bulk, fun () -> Writer.write oc bulk))

    let decoder = Some Reader.read
  end
end

module IO = struct
  module Default = struct
    type 'a t = 'a

    let return a = a
    let ( >>= ) a f = f a
    let catch a b = try a () with exc -> b exc
  end
end

module Buffer_output = struct
  module IO = IO.Default

  type oc = Buffer.t

  let write oc s = Buffer.add_string oc s
end

module String_output = struct
  module IO = IO.Default

  type oc = string ref

  let write oc s = oc := !oc ^ s
end

module String_input = struct
  module IO = IO.Default

  type ic = string ref

  let read input i =
    let s = String.sub !input 0 i in
    input := String.sub !input i (String.length !input - i);
    s

  let read_char input =
    let c = read input 1 in
    c.[0]

  let read_line t =
    let rec aux output =
      match read t 1 with
      | "\n" ->
        output
      | "\r" ->
        aux output
      | c ->
        aux (output ^ c)
    in
    aux ""
end

module Buffer_writer = Writer (Buffer_output)
module String_reader = Reader (String_input)
module String_writer = Writer (String_output)
module String = Make (Bulk.String (String_reader) (String_writer))

let is_nil = function
  | `Nil ->
    true
  | _ ->
    false

let to_value = function
  | `Bulk (`Value v) ->
    Ok v
  | _ ->
    Error `Invalid_value

let to_value_exn x = to_value x |> unwrap

let to_string = function
  | `String s ->
    Ok s
  | `Bulk (`String s) ->
    Ok s
  | `Error e ->
    Ok e
  | `Nil ->
    Ok "nil"
  | `Integer i ->
    Ok (Int64.to_string i)
  | _ ->
    Error `Invalid_value

let to_string_exn x = to_string x |> unwrap

let to_string_or_value x =
  match to_string x with
  | Ok s ->
    Ok (`String s)
  | Error _ -> (
    match to_value x with
    | Ok v ->
      Ok (`Value v)
    | Error e ->
      Error e )

let to_string_or_value_exn x = to_string_or_value x |> unwrap

let to_integer = function
  | `Integer i ->
    Ok i
  | `String s
  | `Bulk (`String s) -> (
    try Ok (Int64.of_string s) with _ -> Error `Invalid_value )
  | _ ->
    Error `Invalid_value

let to_integer_exn x = to_integer x |> unwrap

let to_float = function
  | `Integer i ->
    Ok (Int64.to_float i)
  | `String s
  | `Bulk (`String s) -> (
    try Ok (float_of_string s) with _ -> Error `Invalid_value )
  | _ ->
    Error `Invalid_value

let to_float_exn x = to_float x |> unwrap

let to_array f = function
  | `Array a ->
    Ok (Array.map f a)
  | _ ->
    Error `Invalid_value

let to_array_exn f x = to_array f x |> unwrap

let to_alist k v = function
  | `Array a ->
    let len = Array.length a in
    if len mod 2 <> 0 then Error `Invalid_value
    else
      let dest = ref [] in
      Array.iteri
        (fun i x -> if i < len - 1 then dest := (k x, v a.(i + 1)) :: !dest)
        a;
      Ok !dest
  | _ ->
    Error `Invalid_value

let to_alist_exn k v x = to_alist k v x |> unwrap

(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
