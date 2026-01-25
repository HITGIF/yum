open! Core
open! Async

module Event : sig
  type t =
    | Voice_ready of
        { channel_id : Model.Channel_id.t
        ; frames_writer : Audio.Pcm_frame.t Queue.t Pipe.Writer.t
        }
end

type t

val create
  :  ?time_source:[> read ] Time_source.T1.t
  -> token:Model.Voice_connection_token.t
  -> endpoint:Model.Uri.t
  -> guild_id:Model.Guild_id.t
  -> channel_id:Model.Channel_id.t
  -> session_id:Model.Voice_gateway_session_id.t
  -> user_id:Model.User_id.t
  -> reincarnate:(unit -> unit Deferred.t)
  -> get_users_in_channel:(unit -> Model.User_id.t list)
  -> unit
  -> t

val connect : t -> unit Deferred.Or_error.t
val close : t -> unit Deferred.t
val events : t -> Event.t Pipe.Reader.t
val session_id : t -> Model.Voice_gateway_session_id.t
val token : t -> Model.Voice_connection_token.t
val endpoint : t -> Model.Uri.t
