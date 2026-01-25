open! Core

val max_supported_protocol_version : int

module Uint8_data : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
end

module Uint64_data : sig
  type t

  val len : t -> Unsigned.size_t
end

module Commit_result : sig
  type t

  val destroy : t -> unit
  val is_failed : t -> bool
  val is_ignored : t -> bool
  val get_roster_member_ids : t -> Uint64_data.t
  val get_roster_member_signature : t -> int -> Uint8_data.t
end

module Welcome_result : sig
  type t

  val destroy : t -> unit
  val get_roster_member_ids : t -> Uint64_data.t
  val get_roster_member_signature : t -> int -> Uint8_data.t
end

module Key_ratchet : sig
  type t

  val destroy : t -> unit
end

module Session : sig
  type nonrec t

  val create : on_error:(source:string -> reason:string -> unit) -> t
  val destroy : t -> unit
  val init : t -> version:int -> group_id:int -> self_user_id:string -> unit
  val reset : t -> unit
  val set_protocol_version : t -> version:int -> unit
  val get_protocol_version : t -> int
  val get_last_epoch_authenticator : t -> Uint8_data.t
  val set_external_sender : t -> Uint8_data.t -> unit

  val process_proposals
    :  t
    -> proposals:Uint8_data.t
    -> recognized_user_ids:string list
    -> Uint8_data.t

  val process_commit : t -> Uint8_data.t -> Commit_result.t

  val process_welcome
    :  t
    -> Uint8_data.t
    -> recognized_user_ids:string list
    -> Welcome_result.t

  val get_marshalled_key_package : t -> Uint8_data.t
  val get_key_ratchet : t -> string -> Key_ratchet.t
end

module Codec : sig
  type t =
    | Unknown
    | Opus
    | Vp8
    | Vp9
    | H264
    | H265
    | Av1
end

module Media_type : sig
  type t =
    | Audio
    | Video
end

module Encryptor_result_code : sig
  type t =
    | Success
    | Encryption_failure
    | Missing_key_ratchet
    | Missing_cryptor
    | Too_many_attempts
  [@@deriving sexp_of]
end

module Decryptor_result_code : sig
  type t =
    | Success
    | Decryption_failure
    | Missing_key_ratchet
    | Invalid_nonce
    | Missing_cryptor
  [@@deriving sexp_of]
end

module Encryptor_stats : sig
  type t

  val t : t Ctypes.structure Ctypes.typ
end

module Decryptor_stats : sig
  type t

  val t : t Ctypes.structure Ctypes.typ
end

module Encryptor : sig
  type t

  val create : unit -> t
  val destroy : t -> unit
  val set_key_ratchet : t -> Key_ratchet.t -> unit
  val set_passthrough_mode : t -> bool -> unit
  val assign_ssrc_to_codec : t -> ssrc:int -> codec:Codec.t -> unit
  val get_protocol_version : t -> int
  val has_key_ratchet : t -> bool
  val is_passthrough_mode : t -> bool

  val encrypt
    :  t
    -> media_type:Media_type.t
    -> ssrc:int
    -> plaintext:bytes
    -> Encryptor_result_code.t * bytes

  val set_protocol_version_changed_callback : t -> callback:(unit -> unit) -> unit
  val get_stats : t -> media_type:Media_type.t -> Encryptor_stats.t Ctypes.structure
end

module Decryptor : sig
  type t

  val create : unit -> t
  val destroy : t -> unit
  val transition_to_key_ratchet : t -> Key_ratchet.t -> unit
  val set_passthrough_mode : t -> bool -> unit

  val decrypt
    :  t
    -> media_type:Media_type.t
    -> ciphertext:bytes
    -> Decryptor_result_code.t * bytes

  val get_stats : t -> media_type:Media_type.t -> Decryptor_stats.t Ctypes.structure
end
