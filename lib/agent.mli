open! Core
open! Async

module Emoji : sig
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
    | Arrow_forward
    | Arrow_up
    | Arrow_double_up
    | Fast_forward
    | Repeat
    | Stop_button
    | Wave

  val to_name : t -> string
  val to_unicode : t -> string
end

module Action : sig
  type t =
    | Skip
    | Stop
    | Start
    | Play of Song.t
    | Play_now of Song.t
    | Unknown of string

  val of_custom_id : string -> t
end

module Button : sig
  module Style : sig
    type t =
      | Primary
      | Secondary
      | Success
      | Danger
  end

  type t =
    { style : Style.t
    ; action : Action.t
    ; label : string option
    ; emoji : Emoji.t option
    }
end

type t

val create
  :  auth_token:Discord.Model.Auth_token.t
  -> channel_id:Discord.Model.Channel_id.t
  -> t

val send_message'
  :  ?buttons:Button.t list
  -> ?code:unit
  -> ?emoji:Emoji.t
  -> ?emoji_end:Emoji.t
  -> t
  -> string option
  -> unit Deferred.t

val send_message
  :  ?buttons:Button.t list
  -> ?code:unit
  -> ?emoji:Emoji.t
  -> ?emoji_end:Emoji.t
  -> t
  -> string
  -> unit Deferred.t

val respond_interaction
  :  ?emoji:Emoji.t
  -> ?emoji_end:Emoji.t
  -> t
  -> Discord.Model.Interaction_id.t
  -> Discord.Model.Interaction_token.t
  -> string
  -> unit Deferred.t

val register_slash_commands
  :  auth_token:Discord.Model.Auth_token.t
  -> application_id:Discord.Model.User_id.t
  -> Discord.Model.Slash_command.t list
  -> unit Deferred.t
