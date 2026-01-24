open! Core

val max_supported_protocol_version : int

module Uint8_data : sig
  type t
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
end
