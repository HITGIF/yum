open! Core

type t [@@deriving sexp_of]

val create : ?reset_after:Time_ns.Span.t -> max:int -> unit -> t
val try_ : t -> unit Or_error.t
val reset : t -> unit

(** The maximum number of attempts allowed. *)
val max : t -> int

(** How many attempts have been consumed since the last reset (e.g. [1] right
    after the first successful {!try_}). *)
val attempt_number : t -> int
