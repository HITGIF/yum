open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
include Yojson.Safe

type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
  | `Tuple of t list
  | `Variant of string * t option
  ]
[@@deriving sexp_of]

let t_of_yojson = Fn.id
let yojson_of_t = Fn.id
