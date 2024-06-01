open! Core

type t

val of_string : string -> t
val to_url : t -> string
val of_url_exn : string -> t
