open! Core

type t

val of_youtube_string : string -> t
val to_url : t -> string
val to_src : t -> [> `Bilibili of string | `Ytdl of string ]
val of_url : string -> t Or_error.t
val supported_url_formats_msg : string

module Playlist : sig
  type t

  val to_url : t -> string
  val to_src : t -> [> `Ytdl_playlist of string ]
  val of_url : string -> t Or_error.t
  val supported_url_formats_msg : string
end
