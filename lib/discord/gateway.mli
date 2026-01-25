open! Core
open! Async

module Event : sig
  type t =
    | Message of Model.Message.t
    | Interaction of
        { id : Model.Interaction_id.t
        ; token : Model.Interaction_token.t
        ; guild_id : Model.Guild_id.t
        ; custom_id : string
        ; component_type : int
        }
    | Voice_connected of { guild_id : Model.Guild_id.t }
    | Voice of
        { guild_id : Model.Guild_id.t
        ; event : Voice_gateway.Event.t
        }
end

type t

val with_
  :  ?time_source:[> read ] Time_source.T1.t
  -> initial_gateway_url:Model.Uri.t
  -> auth_token:Model.Auth_token.t
  -> intents:Model.Intents.Intent.t list
  -> properties:Model.Gateway.Event.Identify.Properties.t
  -> (t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t

val events : t -> Event.t Pipe.Reader.t

val join_voice
  :  t
  -> guild_id:Model.Guild_id.t
  -> channel_id:Model.Channel_id.t
  -> unit Deferred.t

val join_user_voice
  :  t
  -> guild_id:Model.Guild_id.t
  -> user_id:Model.User_id.t
  -> [ `Ok of Model.Channel_id.t | `User_not_in_voice_channel ] Deferred.t

val leave_voice : t -> guild_id:Model.Guild_id.t -> unit Deferred.t
val reconnect_voice : t -> guild_id:Model.Guild_id.t -> unit Deferred.t
