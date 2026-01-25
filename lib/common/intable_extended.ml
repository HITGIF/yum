open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
include Intable_extended_intf

module Make () = struct
  type t = int [@@deriving compare, equal, hash, sexp_of, yojson, quickcheck]

  let of_int_exn = Fn.id
  let to_int_exn = Fn.id

  include functor Comparable.Make_plain
  include functor Hashable.Make_plain
end
