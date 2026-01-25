open! Core
open! Async

module Emoji : sig
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
end

type t

val create : auth_token:Model.Auth_token.t -> channel_id:Model.Channel_id.t -> t

val send_message
  :  ?code:unit
  -> ?emoji:Emoji.t
  -> ?emoji_end:Emoji.t
  -> t
  -> string
  -> unit Deferred.t
