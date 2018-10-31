(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** REdis Serialization Protocol library for OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Resp} *)

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

val pp_error : Format.formatter -> error -> unit
val string_of_error : error -> string
val unwrap : ('a, error) result -> 'a

exception Exc of error

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
  val read : Reader.ic -> bulk t IO.t
  val read_s : Reader.ic -> bulk t IO.t
end

module Reader (I : INPUT) : READER with type ic = I.ic and module IO = I.IO
module Writer (O : OUTPUT) : WRITER with type oc = O.oc and module IO = O.IO

module Make (Bulk : BULK) :
  S
  with module IO = Bulk.IO
   and module Writer.IO = Bulk.IO
   and module Reader = Bulk.Reader
   and module Writer = Bulk.Writer
   and type bulk = Bulk.bulk

module IO : sig
  module Default : IO with type 'a t = 'a
end

module Buffer_output : OUTPUT with type oc = Buffer.t and type 'a IO.t = 'a
module String_output : OUTPUT with type oc = string ref and type 'a IO.t = 'a
module String_input : INPUT with type 'a IO.t = 'a and type ic = string ref

module Buffer_writer :
  WRITER with type oc = Buffer_output.oc and type 'a IO.t = 'a

module String_reader :
  READER with type ic = String_input.ic and type 'a IO.t = 'a

module String_writer :
  WRITER with type oc = String_output.oc and type 'a IO.t = 'a

module Bulk : sig
  module String (R : READER) (W : WRITER with module IO = R.IO) :
    BULK
    with module IO = R.IO
     and module Reader = R
     and module Writer = W
     and type bulk = string
end

module String :
  S
  with type 'a IO.t = 'a
   and module Reader = String_reader
   and module Writer = String_writer
   and type bulk = string

val is_nil : 'a t -> bool
val to_value : 'a t -> ('a, error) result
val to_value_exn : 'a t -> 'a
val to_string : 'a t -> (string, error) result
val to_string_exn : 'a t -> string

val to_string_or_value :
  'a t -> ([`String of string | `Value of 'a], error) result

val to_string_or_value_exn : 'a t -> [`String of string | `Value of 'a]
val to_integer : 'a t -> (int64, error) result
val to_integer_exn : 'a t -> int64
val to_float : 'a t -> (float, error) result
val to_float_exn : 'a t -> float
val to_array : ('a t -> 'b) -> 'a t -> ('b array, error) result
val to_array_exn : ('a t -> 'b) -> 'a t -> 'b array

val to_alist :
  ('a t -> 'k) -> ('a t -> 'v) -> 'a t -> (('k * 'v) list, error) result

val to_alist_exn : ('a t -> 'k) -> ('a t -> 'v) -> 'a t -> ('k * 'v) list
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
