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
    | Mag
    | Clipboard
    | Regional_indicator_y
    | Regional_indicator_b

  val to_name : t -> string
  val to_unicode : t -> string
end

module Custom_emoji : sig
  (** A Discord custom (server/application) emoji. *)
  type t =
    { name : string
    ; id : string
    ; animated : bool
    }

  (** [of_string s] parses the Discord chat form ["<:name:id>"] (or
      ["<a:name:id>"] for animated). *)
  val of_string : string -> t Or_error.t
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

module Select : sig
  module Option : sig
    type t =
      { label : string
      ; description : string option
      ; emoji : [ `Unicode of Emoji.t | `Custom of Custom_emoji.t ] option
      ; action : Action.t
      }
  end
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

(** [send_select t message options] posts [message] followed by a single-choice
    dropdown of [options]; selecting one triggers a message-component interaction
    carrying that option's [action]. *)
val send_select
  :  ?emoji:Emoji.t
  -> ?placeholder:string
  -> t
  -> string
  -> Select.Option.t list
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
