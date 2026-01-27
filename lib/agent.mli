open! Core
open! Async

module Emoji : sig
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
    | Arrow_forward
    | Arrow_double_up
    | Fast_forward
    | Repeat
end

module Action : sig
  type t =
    | Skip
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
