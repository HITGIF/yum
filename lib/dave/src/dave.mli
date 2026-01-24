open! Core

val max_supported_protocol_version : int

module Uint8_data : sig
  type t
end

module Uint64_data : sig
  type t
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
  type t

  val create : on_error:(source:string -> reason:string -> unit) -> t
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
