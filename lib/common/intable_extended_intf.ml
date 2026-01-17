open! Core

module type S = sig
  type t [@@deriving sexp_of, yojson]

  include Core.Intable.S with type t := t
end

module type Intable_extended = sig
  module type S = S

  module Make () : S
end
