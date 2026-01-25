open! Core

module type S = sig
  type t [@@deriving compare, equal, hash, sexp_of, yojson]

  include Core.Intable.S with type t := t
  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module type Intable_extended = sig
  module type S = S

  module Make () : S
end
