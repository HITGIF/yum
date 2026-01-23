open! Core

type t [@@deriving sexp_of]

val create : ?reset_after:Time_ns.Span.t -> max:int -> unit -> t
val try_ : t -> unit Or_error.t
val reset : t -> unit
