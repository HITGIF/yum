open! Core
module Intable_extended := Common.Intable_extended
module Json := Common.Json

(*_ *)
module Auth_token : String_id.S
module Channel_id : String_id.S
module Guild_id : String_id.S
module Message_id : String_id.S
module Gateway_session_id : String_id.S
module Voice_gateway_session_id : String_id.S
module User_id : String_id.S
module Voice_connection_token : String_id.S

module Uri : sig
  include module type of Uri

  val to_uri : t -> Uri.t
  val of_uri : Uri.t -> t
  val to_string : t -> string

  include Sexpable.S with type t := t
end

module Ssrc : Intable_extended.S

module Intents : sig
  (** https://discord.com/developers/docs/topics/gateway#gateway-intents *)
  type t [@@deriving sexp_of]

  module Intent : sig
    type t =
      | Guilds
      | Guild_members
      | Guild_moderation
      | Guild_emojis_and_stickers
      | Guild_integrations
      | Guild_webhooks
      | Guild_invites
      | Guild_voice_states
      | Guild_presences
      | Guild_messages
      | Guild_message_reactions
      | Guild_message_typing
      | Direct_messages
      | Direct_message_reactions
      | Direct_message_typing
      | Message_content
      | Guild_scheduled_events
      | Auto_moderation_configuration
      | Auto_moderation_execution
      | Guild_message_polls
      | Direct_message_polls
    [@@deriving sexp_of]
  end

  val create : Intent.t list -> t
end

module User : sig
  (** https://discord.com/developers/docs/resources/user#user-object *)
  type t =
    { id : User_id.t
    ; username : string
    ; global_name : string option
    ; bot : bool option
    }
  [@@deriving sexp_of]
end

module Message : sig
  module Type : sig
    (** https://discord.com/developers/docs/resources/channel#message-object-message-types *)
    type t =
      | Default
      | Other of int
    [@@deriving sexp_of]

    include Intable.S with type t := t
  end

  (** https://discord.com/developers/docs/resources/channel#message-object *)
  type t =
    { id : Message_id.t
    ; guild_id : Guild_id.t option
    ; channel_id : Channel_id.t
    ; author : User.t
    ; content : string
    ; timestamp : string
    ; edited_timestamp : string option
    ; type_ : Type.t
    }
  [@@deriving sexp_of]
end

module Encryption_mode : sig
  type t =
    [ `Aead_xchacha20_poly1305_rtpsize
    | `Unsupported of Json.t
    ]
  [@@deriving equal, sexp_of]
end

(** https://discord.com/developers/docs/events/gateway *)
module Gateway : sig
  module Api_version : sig
    (** https://discord.com/developers/docs/reference#api-versioning *)
    type t [@@deriving sexp_of]

    val v10 : t
    val rest_url : t -> Uri.t
    val ws_url : t -> base:Uri.t -> Uri.t

    include Intable.S with type t := t
  end

  (** https://discord.com/developers/docs/events/gateway-events *)
  module Event : sig
    module Hello : sig
      type t = { heartbeat_interval : Time_ns.Span.t } [@@deriving sexp_of]
    end

    module Heartbeat : sig
      type t = { last_seq_num : Websocket_protocol.Seq_num.t option } [@@deriving sexp_of]
    end

    module Identify : sig
      module Properties : sig
        type t =
          { os : string
          ; browser : string
          ; device : string
          }
        [@@deriving sexp_of]
      end

      type t =
        { token : Auth_token.t
        ; intents : Intents.t
        ; properties : Properties.t
        }
      [@@deriving sexp_of]
    end

    module Dispatch : sig
      module Ready : sig
        type t =
          { v : Api_version.t
          ; user : User.t
          ; guilds : Json.t list
          ; session_id : Gateway_session_id.t
          ; resume_gateway_url : Uri.t
          ; application : Json.t
          }
        [@@deriving sexp_of]
      end

      module Voice_state : sig
        (** https://discord.com/developers/docs/resources/voice#voice-state-object *)
        type t =
          { guild_id : Guild_id.t option
          ; channel_id : Channel_id.t option
          ; user_id : User_id.t
          ; session_id : Voice_gateway_session_id.t
          }
        [@@deriving sexp_of]
      end

      module Guild_create : sig
        type t =
          { id : Guild_id.t
          ; name : string option
          ; unavailable : bool option
          ; voice_states : Voice_state.t list
          }
        [@@deriving sexp_of]
      end

      module Voice_server_update : sig
        type t =
          { token : Voice_connection_token.t
          ; guild_id : Guild_id.t
          ; endpoint : Uri.t option
          }
        [@@deriving sexp_of]
      end

      type t =
        | Ready of Ready.t
        (** https://discord.com/developers/docs/events/gateway-events#ready *)
        | Resumed (** https://discord.com/developers/docs/events/gateway-events#resumed *)
        | Guild_create of Guild_create.t
        (** https://discord.com/developers/docs/events/gateway-events#guild-create *)
        | Message_create of Message.t
        (** https://discord.com/developers/docs/events/gateway-events#message-create *)
        | Voice_state_update of Voice_state.t
        (** https://discord.com/developers/docs/events/gateway-events#voice-state-update *)
        | Voice_server_update of Voice_server_update.t
        (** https://discord.com/developers/docs/events/gateway-events#voice-server-update *)
        | Unknown of
            { name : string
            ; data : Json.t
            }
      [@@deriving sexp_of]
    end

    module Invalid_session : sig
      type t = { resumable : bool } [@@deriving sexp_of]
    end

    module Resume : sig
      type t =
        { token : Auth_token.t
        ; session_id : Gateway_session_id.t
        ; seq : Websocket_protocol.Seq_num.t option
        }
      [@@deriving sexp_of]
    end

    module Voice_state_update : sig
      type t =
        { guild_id : Guild_id.t
        ; channel_id : Channel_id.t option
        ; self_mute : bool
        ; self_deaf : bool
        }
      [@@deriving sexp_of]
    end

    module Receivable : sig
      (** https://discord.com/developers/docs/events/gateway-events#receive-events *)
      type t =
        | Dispatch of Dispatch.t
        | Heartbeat
        (** https://discord.com/developers/docs/events/gateway-events#heartbeat *)
        | Reconnect
        (** https://discord.com/developers/docs/events/gateway-events#reconnect *)
        | Invalid_session of Invalid_session.t
        (** https://discord.com/developers/docs/events/gateway-events#invalid-session *)
        | Hello of Hello.t
        (** https://discord.com/developers/docs/events/gateway-events#hello *)
        | Heartbeat_ack
        | Unknown of Websocket_protocol.Gateway.Event.t
      [@@deriving sexp_of]

      val of_protocol_or_error : Websocket_protocol.Gateway.Event.t -> t Or_error.t
    end

    module Sendable : sig
      (** https://discord.com/developers/docs/events/gateway-events#send-events *)
      type t =
        | Heartbeat of Heartbeat.t
        (** https://discord.com/developers/docs/events/gateway-events#heartbeat *)
        | Identify of Identify.t
        (** https://discord.com/developers/docs/events/gateway-events#identify *)
        | Resume of Resume.t
        (** https://discord.com/developers/docs/events/gateway-events#resume *)
        | Voice_state_update of Voice_state_update.t
        (** https://discord.com/developers/docs/topics/voice-connections#retrieving-voice-server-information-gateway-voice-state-update-example *)
      [@@deriving sexp_of]

      val to_protocol : t -> Websocket_protocol.Gateway.Event.t
    end
  end
end

(** https://discord.com/developers/docs/topics/voice-connections *)
module Voice_gateway : sig
  module Api_version : sig
    (** https://discord.com/developers/docs/topics/voice-connections#voice-gateway-versioning *)
    type t [@@deriving sexp_of]

    val v8 : t
    val ws_url : t -> base:Uri.t -> Uri.t

    include Intable.S with type t := t
  end

  module Event : sig
    module Identify : sig
      type t =
        { server_id : Guild_id.t
        ; user_id : User_id.t
        ; session_id : Voice_gateway_session_id.t
        ; token : Voice_connection_token.t
        ; max_dave_protocol_version : int
        }
      [@@deriving sexp_of]
    end

    module Ready : sig
      type t =
        { ssrc : Ssrc.t
        ; ip : string
        ; port : int
        ; modes : Encryption_mode.t list
        }
      [@@deriving sexp_of]
    end

    module Hello : sig
      type t = { heartbeat_interval : Time_ns.Span.t } [@@deriving sexp_of]
    end

    module Heartbeat : sig
      type t =
        { nonce : int
        ; seq_ack : Websocket_protocol.Seq_num.t option
        }
      [@@deriving sexp_of]
    end

    module Heartbeat_ack : sig
      type t = { nonce : string } [@@deriving sexp_of]
    end

    module Select_protocol : sig
      module Data : sig
        type t =
          { address : string
          ; port : int
          ; mode : Encryption_mode.t
          }
        [@@deriving sexp_of]
      end

      type t =
        { protocol : string
        ; data : Data.t
        }
      [@@deriving sexp_of]
    end

    module Resume : sig
      type t =
        { server_id : Guild_id.t
        ; session_id : Voice_gateway_session_id.t
        ; token : Voice_connection_token.t
        ; seq_ack : Websocket_protocol.Seq_num.t option
        }
      [@@deriving sexp_of]
    end

    module Session_description : sig
      type t =
        { mode : Encryption_mode.t
        ; secret_key : int array
        ; dave_protocol_version : int
        }
      [@@deriving sexp_of]
    end

    module Speaking : sig
      type t =
        { speaking : int
        ; delay : int
        ; ssrc : Ssrc.t
        }
    end

    module Receivable : sig
      type t =
        | Ready of Ready.t
        (** https://discord.com/developers/docs/topics/voice-connections#establishing-a-voice-websocket-connection-example-voice-ready-payload *)
        | Hello of Hello.t
        (** https://discord.com/developers/docs/topics/voice-connections#heartbeating-example-hello-payload *)
        | Heartbeat_ack of Heartbeat_ack.t
        (** https://discord.com/developers/docs/topics/voice-connections#heartbeating-example-heartbeat-ack-payload-since-v8 *)
        | Session_description of Session_description.t
        (** https://discord.com/developers/docs/topics/voice-connections#transport-encryption-modes-example-session-description-payload *)
        | Unknown of Websocket_protocol.Voice_gateway.Event.t
      [@@deriving sexp_of]

      val of_protocol_or_error : Websocket_protocol.Voice_gateway.Event.t -> t Or_error.t
    end

    module Sendable : sig
      type t =
        | Identify of Identify.t
        (** https://discord.com/developers/docs/topics/voice-connections#establishing-a-voice-websocket-connection-example-voice-identify-payload *)
        | Heartbeat of Heartbeat.t
        (** https://discord.com/developers/docs/topics/voice-connections#heartbeating-example-heartbeat-payload-since-v8 *)
        | Select_protocol of Select_protocol.t
        (** https://discord.com/developers/docs/topics/voice-connections#establishing-a-voice-udp-connection-example-select-protocol-payload *)
        | Resume of Resume.t
        (** https://discord.com/developers/docs/topics/voice-connections#resuming-voice-connection-example-resume-connection-payload-since-v8 *)
        | Speaking of Speaking.t
        (** https://discord.com/developers/docs/topics/voice-connections#speaking *)
      [@@deriving sexp_of]

      val to_protocol : t -> Websocket_protocol.Voice_gateway.Event.t
    end
  end
end
