open! Core
open! Ctypes

val to_u16 : int -> Unsigned.uint16
val of_u16 : Unsigned.uint16 -> int
val to_u32 : int -> Unsigned.uint32
val of_u32 : Unsigned.uint32 -> int
val to_u64 : int -> Unsigned.uint64
val of_u64 : Unsigned.uint64 -> int
val to_ull : int -> Unsigned.ullong
val of_ull : Unsigned.ullong -> int
val to_size_t : int -> Unsigned.size_t
val of_size_t : Unsigned.size_t -> int
val ( !! ) : bytes -> bytes ocaml
val ( !!? ) : bytes option -> bytes ocaml
val ( !? ) : bytes -> Unsigned.ullong
val ( !?? ) : bytes option -> Unsigned.ullong
val validate_len : here:[%call_pos] -> expect:int -> bytes -> unit Or_error.t
