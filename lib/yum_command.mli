open! Core

type t =
  | Ping
  | Start
  | Stop
  | Skip
  | Help
  | Play of Song.t
  | Play_now of Song.t
  | Play_list of Song.Playlist.t
[@@deriving variants]

val parse : string -> t option Or_error.t
val help_text : string
