open! Core
module Intable_extended = Common.Intable_extended
module Json = Common.Json
module Seq_num = Intable_extended.Make ()

module type S = sig
  module Op_code : sig
    type t [@@deriving equal]
  end

  module Event : sig
    type t [@@deriving sexp_of]

    val create : ?data:string -> ?name:string -> Op_code.t -> t
    val of_frame_content_or_error : Websocket.Frame_content.t -> t Or_error.t
    val to_frame_content : t -> Websocket.Frame_content.t
    val op_code : t -> Op_code.t
    val data : t -> string option
    val seq_num : t -> Seq_num.t option
    val name : t -> string option
    val data_or_error : t -> string Or_error.t
    val data_json_or_error : t -> Json.t Or_error.t
    val seq_num_or_error : t -> Seq_num.t Or_error.t
    val name_or_error : t -> string Or_error.t
  end
end

module Op_code = struct
  module Gateway = struct
    (** https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-opcodes *)
    type t =
      | Dispatch
      | Heartbeat
      | Identify
      | Presence_update
      | Voice_state_update
      | Resume
      | Reconnect
      | Request_guild_members
      | Invalid_session
      | Hello
      | Heartbeat_ack
      | Request_soundboard_sounds
      | Unknown of int
    [@@deriving equal]
  end

  module Voice_gateway = struct
    (** https://discord.com/developers/docs/topics/opcodes-and-status-codes#voice-voice-opcodes *)
    type t =
      | Identify
      | Select_protocol
      | Ready
      | Heartbeat
      | Session_description
      | Speaking
      | Heartbeat_ack
      | Resume
      | Hello
      | Resumed
      | Clients_connect
      | Client_disconnect
      | Dave_protocol_prepare_transition
      | Dave_protocol_execute_transition
      | Dave_protocol_ready_for_transition
      | Dave_protocol_prepare_epoch
      | Mls_external_sender_package
      | Mls_key_package
      | Mls_proposals
      | Mls_commit_welcome
      | Mls_announce_commit_transition
      | Mls_welcome
      | Mls_invalid_commit_welcome
      | Unknown of int
    [@@deriving equal]
  end
end

module type Websocket_protocol = sig
  module type S = S

  module Seq_num : Intable_extended.S with type t = Seq_num.t
  module Gateway : S with type Op_code.t = Op_code.Gateway.t
  module Voice_gateway : S with type Op_code.t = Op_code.Voice_gateway.t
end
