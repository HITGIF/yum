open! Core
open! Async

(** Outgoing messages to be sent to the voice gateway *)
module Outgoing : sig
  type t =
    | Mls_key_package of { key_package : string }
    | Dave_protocol_ready_for_transition of { transition_id : int }
    | Mls_commit_welcome of { commit_welcome : string }
    | Mls_invalid_commit_welcome of { transition_id : int }
  [@@deriving sexp_of]
end

type t

val create : self_user_id:string -> group_id:int -> t
val close : t -> unit

(** Outgoing messages pipe - messages that should be sent to voice gateway *)
val outgoing : t -> Outgoing.t Pipe.Reader.t

val on_clients_connect : t -> Model.Voice_gateway.Event.Clients_connect.t -> unit
val on_client_disconnect : t -> Model.Voice_gateway.Event.Client_disconnect.t -> unit
val on_session_description : t -> dave_protocol_version:int -> unit

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

(** Encrypt a frame for sending. Writes to [output] buffer, returns bytes written. *)
val encrypt
  :  t
  -> ssrc:Model.Ssrc.t
  -> frame:bytes
  -> output:bytes
  -> Dave.Encryptor_result_code.t * int

(** Get max ciphertext size for a given frame size *)
val get_max_ciphertext_byte_size : t -> frame_size:int -> int

(** Decrypt a received frame. Writes to [output] buffer, returns bytes written. *)
val decrypt
  :  t
  -> encrypted_frame:bytes
  -> output:bytes
  -> Dave.Decryptor_result_code.t * int

(** Get max plaintext size for a given encrypted frame size *)
val get_max_plaintext_byte_size : t -> encrypted_frame_size:int -> int

(** Assign SSRC to codec for encryption *)
val assign_ssrc_to_codec : t -> ssrc:Model.Ssrc.t -> codec:Dave.Codec.t -> unit
