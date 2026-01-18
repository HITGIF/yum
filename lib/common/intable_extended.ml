open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
include Intable_extended_intf

module Make () = struct
  type t = int [@@deriving equal, sexp_of, yojson, quickcheck]

  let of_int_exn = Fn.id
  let to_int_exn = Fn.id
end
