open! Core
include module type of Yojson.Safe

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

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
