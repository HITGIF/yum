open! Core
open! Async

module Emoji : sig
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
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
    ; custom_id : string
    ; label : string option
    }
end

type t

val create : auth_token:Model.Auth_token.t -> channel_id:Model.Channel_id.t -> t

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
