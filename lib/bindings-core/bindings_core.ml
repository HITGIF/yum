open! Core
open! Ctypes

let to_u16 = Unsigned.UInt16.of_int
let of_u16 = Unsigned.UInt16.to_int
let to_u32 = Unsigned.UInt32.of_int
let of_u32 = Unsigned.UInt32.to_int
let to_u64 = Unsigned.UInt64.of_int
let of_u64 = Unsigned.UInt64.to_int
let to_ull = Unsigned.ULLong.of_int
let of_ull = Unsigned.ULLong.to_int
let to_size_t = Unsigned.Size_t.of_int
let of_size_t = Unsigned.Size_t.to_int

(*_ *)
let ( !! ) bytes = ocaml_bytes_start bytes

let ( !!? ) =
  let null = Bytes.create 0 in
  function
  | None -> !!null
  | Some bytes -> !!bytes
;;

let ( !? ) bytes = to_ull (Bytes.length bytes)

let ( !?? ) = function
  | None -> to_ull 0
  | Some bytes -> !?bytes
;;

let validate_len ~(here : [%call_pos]) ~expect bytes =
  let actual = Bytes.length bytes in
  if actual = expect
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Unexpected length" (expect : int) (actual : int) (here : Source_code_position.t)]
;;
