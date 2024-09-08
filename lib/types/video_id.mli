open! Core

type t

val of_youtube_string : string -> t
val to_url : t -> string
val of_url : string -> t Or_error.t
val supported_url_formats_msg : string
