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

(** Key ratchet update notification *)
module Key_ratchet_update : sig
  type t =
    { user_id : string
    ; key_ratchet : Dave.Key_ratchet.t option
    }
end

type t

(** Create a new DAVE session manager *)
val create : self_user_id:string -> group_id:int -> t

(** Outgoing messages pipe - messages that should be sent to voice gateway *)
val outgoing : t -> Outgoing.t Pipe.Reader.t

(** Key ratchet updates pipe - notifications when key ratchets change *)
val key_ratchet_updates : t -> Key_ratchet_update.t Pipe.Reader.t

(** Add an allowed user to the connection *)
val create_user : t -> user_id:string -> unit

(** Remove an allowed user from the connection *)
val destroy_user : t -> user_id:string -> unit

(** Handle Session_description - called with dave_protocol_version *)
val on_session_description : t -> dave_protocol_version:int -> unit

(** Handle Dave_protocol_prepare_transition (opcode 21) *)
val on_dave_protocol_prepare_transition
  :  t
  -> transition_id:int
  -> protocol_version:int
  -> unit

(** Handle Dave_protocol_execute_transition (opcode 22) *)
val on_dave_protocol_execute_transition : t -> transition_id:int -> unit

(** Handle Dave_protocol_prepare_epoch (opcode 24) *)
val on_dave_protocol_prepare_epoch : t -> epoch:int -> protocol_version:int -> unit

(** Handle Mls_external_sender_package (opcode 25) *)
val on_mls_external_sender_package : t -> external_sender_package:string -> unit

(** Handle Mls_proposals (opcode 27) *)
val on_mls_proposals : t -> proposals:string -> unit

(** Handle Mls_prepare_commit_transition (opcode 29) *)
val on_mls_prepare_commit_transition : t -> transition_id:int -> commit:string -> unit

(** Handle Mls_welcome (opcode 30) *)
val on_mls_welcome : t -> transition_id:int -> welcome:string -> unit

(** Encrypt a frame for sending. Writes to [output] buffer, returns bytes written. *)
val encrypt
  :  t
  -> ssrc:int
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
val assign_ssrc_to_codec : t -> ssrc:int -> codec:Dave.Codec.t -> unit

(** Destroy the session and clean up resources *)
val destroy : t -> unit
