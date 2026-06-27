open! Core

type t =
  | Start
  | Stop
  | Skip
  | Play of Song.t
  | Play_now of Song.t
  | Play_list of Song.Playlist.t
  | Search of { query : string }
  | Ping
  | Help
[@@deriving variants]

module Text_command : sig
  val parse : string -> t option Or_error.t
  val help_text : string
end

module Slash_command : sig
  module Slash_command_option :=
    Discord.Model.Gateway.Event.Dispatch.Interaction_create.Slash_command_option

  val all : Discord.Model.Slash_command.t list
  val parse : name:string -> options:Slash_command_option.t list -> t Or_error.t
end
