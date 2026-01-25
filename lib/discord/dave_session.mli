open! Core
open! Async

val max_supported_protocol_version : Model.Dave_protocol_version.t

type t

val create
  :  user_id:Model.User_id.t
  -> channel_id:Model.Channel_id.t
  -> t * Model.Voice_gateway.Event.Sendable.t Pipe.Reader.t

val close : t -> unit
val assign_ssrc_to_codec : t -> ssrc:Model.Ssrc.t -> codec:Dave.Codec.t -> unit
val on_clients_connect : t -> Model.Voice_gateway.Event.Clients_connect.t -> unit
val on_client_disconnect : t -> Model.Voice_gateway.Event.Client_disconnect.t -> unit
val on_session_description : t -> Model.Dave_protocol_version.t -> unit

val on_dave_protocol_prepare_transition
  :  t
  -> Model.Voice_gateway.Event.Dave_protocol_prepare_transition.t
  -> unit

val on_dave_protocol_execute_transition
  :  t
  -> Model.Voice_gateway.Event.Dave_protocol_execute_transition.t
  -> unit

val on_dave_protocol_prepare_epoch
  :  t
  -> Model.Voice_gateway.Event.Dave_protocol_prepare_epoch.t
  -> unit

val on_mls_external_sender_package
  :  t
  -> Model.Voice_gateway.Event.Mls_external_sender_package.t
  -> unit

val on_mls_proposals : t -> Model.Voice_gateway.Event.Mls_proposals.t -> unit

val on_mls_announce_commit_transition
  :  t
  -> Model.Voice_gateway.Event.Mls_announce_commit_transition.t
  -> unit

val on_mls_welcome : t -> Model.Voice_gateway.Event.Mls_welcome.t -> unit
val encrypt : t -> ssrc:Model.Ssrc.t -> plaintext:bytes -> bytes
val decrypt : t -> ciphertext:bytes -> bytes
