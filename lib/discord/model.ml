open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Intable_extended = Common.Intable_extended
module Json = Common.Json

module Yojsonable_of_stringable (M : Stringable.S) = struct
  let yojson_of_t t = t |> M.to_string |> [%yojson_of: string]
  let t_of_yojson t = t |> [%of_yojson: string] |> M.of_string
end

module Yojsonable_of_intable (M : Intable.S) = struct
  let yojson_of_t t = t |> M.to_int_exn |> [%yojson_of: int]
  let t_of_yojson t = t |> [%of_yojson: int] |> M.of_int_exn
end

module Make_string_id
    (M : sig
       val module_name : string
     end)
    () =
struct
  include String_id.Make (M) ()
  include functor Yojsonable_of_stringable
end

module Auth_token = struct
  include
    Make_string_id
      (struct
        let module_name = [%module_name]
      end)
      ()

  let sexp_of_t _ = Sexp.of_string "<redacted>"
end

module Channel_id =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module Guild_id =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module Message_id =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module Gateway_session_id =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module Voice_gateway_session_id =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module User_id =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module Voice_connection_token =
  Make_string_id
    (struct
      let module_name = [%module_name]
    end)
    ()

module Uri = struct
  include Uri

  let to_string t = Uri.to_string t
  let to_uri = Fn.id
  let of_uri = Fn.id

  include functor Sexpable.Of_stringable
  include functor Yojsonable_of_stringable
end

module Ssrc = Intable_extended.Make ()

module Intents = struct
  include Intable_extended.Make ()

  module Intent = struct
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

    let bit = function
      | Guilds -> 0
      | Guild_members -> 1
      | Guild_moderation -> 2
      | Guild_emojis_and_stickers -> 3
      | Guild_integrations -> 4
      | Guild_webhooks -> 5
      | Guild_invites -> 6
      | Guild_voice_states -> 7
      | Guild_presences -> 8
      | Guild_messages -> 9
      | Guild_message_reactions -> 10
      | Guild_message_typing -> 11
      | Direct_messages -> 12
      | Direct_message_reactions -> 13
      | Direct_message_typing -> 14
      | Message_content -> 15
      | Guild_scheduled_events -> 16
      | Auto_moderation_configuration -> 20
      | Auto_moderation_execution -> 21
      | Guild_message_polls -> 24
      | Direct_message_polls -> 25
    ;;
  end

  let create intents =
    List.fold intents ~init:0 ~f:(fun acc piece -> acc lor (1 lsl Intent.bit piece))
    |> of_int_exn
  ;;
end

module User = struct
  type t =
    { id : User_id.t
    ; username : string
    ; global_name : string option [@default None]
    ; bot : bool option [@default None]
    }
  [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
end

module Message = struct
  module Type = struct
    type t =
      | Default
      | Other of int
    [@@deriving sexp_of]

    let of_int_exn = function
      | 0 -> Default
      | n -> Other n
    ;;

    let to_int_exn = function
      | Default -> 0
      | Other n -> n
    ;;

    include functor Yojsonable_of_intable
  end

  type t =
    { id : Message_id.t
    ; guild_id : Guild_id.t option [@default None]
    ; channel_id : Channel_id.t
    ; author : User.t
    ; content : string
    ; timestamp : string
    ; edited_timestamp : string option [@default None]
    ; type_ : Type.t [@key "type"]
    }
  [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
end

module Tls_encryption_mode = struct
  type t =
    [ `Aead_xchacha20_poly1305_rtpsize
    | `Unsupported of Json.t
    ]
  [@@deriving equal, sexp_of, yojson ~capitalize:"snake_case"]

  let yojson_of_t = function
    | `Unsupported json -> json
    | t ->
      (match [%yojson_of: t] t with
       | `List [ json ] -> json
       | json -> raise_s [%message [%here] "Unexpected yojson_of_t" (json : Json.t)])
  ;;

  let t_of_yojson json =
    match Or_error.try_with (fun () -> `List [ json ] |> [%of_yojson: t]) with
    | Ok mode -> mode
    | Error _ -> `Unsupported json
  ;;
end

module Gateway = struct
  module Api_version = struct
    include Intable_extended.Make ()

    let v10 = of_int_exn 10

    let rest_url t =
      Uri.of_string [%string "https://discord.com/api/v%{Int.to_string (to_int_exn t)}"]
    ;;

    let ws_url t ~base =
      base
      |> Fn.flip Uri.with_scheme (Some "wss")
      |> Fn.flip Uri.with_path "/"
      |> Fn.flip
           Uri.with_query
           [ "v", [ Int.to_string (to_int_exn t) ]; "encoding", [ "json" ] ]
    ;;
  end

  module Event = struct
    module Hello = struct
      type t = { heartbeat_interval : Time_ns.Span.t } [@@deriving sexp_of]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Gateway.Event.data_json_or_error event
        in
        match Yojson.Safe.Util.member "heartbeat_interval" data with
        | `Int heartbeat_interval ->
          Ok { heartbeat_interval = Time_ns.Span.of_int_ms heartbeat_interval }
        | data -> Or_error.error_s [%message "Invalid heartbeat_interval" (data : Json.t)]
      ;;
    end

    module Heartbeat = struct
      type t = { last_seq_num : Websocket_protocol.Seq_num.t option } [@@deriving sexp_of]

      let to_protocol t =
        let data =
          let%map.Option seq_num = t.last_seq_num in
          `Int (Websocket_protocol.Seq_num.to_int_exn seq_num) |> Json.to_string
        in
        Websocket_protocol.Gateway.Event.create ?data Heartbeat
      ;;
    end

    module Identify = struct
      module Properties = struct
        type t =
          { os : string
          ; browser : string
          ; device : string
          }
        [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
      end

      type t =
        { token : Auth_token.t
        ; intents : Intents.t
        ; properties : Properties.t
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Gateway.Event.create ~data Identify
      ;;
    end

    module Dispatch = struct
      module Ready = struct
        type t =
          { v : Api_version.t
          ; user : User.t
          ; guilds : Json.t list
          ; session_id : Gateway_session_id.t
          ; resume_gateway_url : Uri.t
          ; application : Json.t
          }
        [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
      end

      module Voice_state = struct
        type t =
          { guild_id : Guild_id.t option [@default None]
          ; channel_id : Channel_id.t option [@default None]
          ; user_id : User_id.t
          ; session_id : Voice_gateway_session_id.t
          }
        [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
      end

      module Guild_create = struct
        type t =
          { id : Guild_id.t
          ; name : string option [@default None]
          ; unavailable : bool option [@default None]
          ; voice_states : Voice_state.t list [@default []]
          }
        [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
      end

      module Voice_server_update = struct
        module Endpoint = struct
          type t = Uri.t [@@deriving sexp_of, yojson_of]

          let t_of_yojson json = "wss://" ^ [%of_yojson: string] json |> Uri.of_string
        end

        type t =
          { token : Voice_connection_token.t
          ; guild_id : Guild_id.t
          ; endpoint : Endpoint.t option [@default None]
          }
        [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
      end

      type t =
        | Ready of Ready.t
        | Resumed
        | Guild_create of Guild_create.t
        | Message_create of Message.t
        | Voice_state_update of Voice_state.t
        | Voice_server_update of Voice_server_update.t
        | Unknown of
            { name : string
            ; data : Json.t
            }
      [@@deriving sexp_of, yojson ~capitalize:"SCREAMING_SNAKE_CASE"]

      let of_protocol_or_error event =
        let%with () = Or_error.try_with_join in
        let%bind.Or_error name = Websocket_protocol.Gateway.Event.name_or_error event in
        let%map.Or_error data =
          Websocket_protocol.Gateway.Event.data_json_or_error event
        in
        match
          Or_error.try_with (fun () -> `List [ `String name; data ] |> [%of_yojson: t])
        with
        | Ok event -> event
        | Error _ ->
          (match
             Or_error.try_with (fun () -> `List [ `String name ] |> [%of_yojson: t])
           with
           | Ok event -> event
           | Error _ -> Unknown { name; data })
      ;;
    end

    module Invalid_session = struct
      type t = { resumable : bool } [@@deriving sexp_of]

      let of_protocol event =
        let resumable =
          match Websocket_protocol.Gateway.Event.data_json_or_error event with
          | Ok (`Bool resumable) -> resumable
          | _ -> false
        in
        { resumable }
      ;;
    end

    module Resume = struct
      type t =
        { token : Auth_token.t
        ; session_id : Gateway_session_id.t
        ; seq : Websocket_protocol.Seq_num.t option
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Gateway.Event.create ~data Resume
      ;;
    end

    module Voice_state_update = struct
      type t =
        { guild_id : Guild_id.t
        ; channel_id : Channel_id.t option [@default None]
        ; self_mute : bool
        ; self_deaf : bool
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Gateway.Event.create ~data Voice_state_update
      ;;
    end

    module Receivable = struct
      type t =
        | Dispatch of Dispatch.t
        | Heartbeat
        | Reconnect
        | Invalid_session of Invalid_session.t
        | Hello of Hello.t
        | Heartbeat_ack
        | Unknown of Websocket_protocol.Gateway.Event.t
      [@@deriving sexp_of, variants]

      let of_protocol_or_error event =
        let open Or_error.Let_syntax in
        match Websocket_protocol.Gateway.Event.op_code event with
        | Dispatch -> Dispatch.of_protocol_or_error event >>| dispatch
        | Heartbeat -> Ok Heartbeat
        | Reconnect -> Ok Reconnect
        | Invalid_session -> Invalid_session.of_protocol event |> invalid_session |> Ok
        | Hello -> Hello.of_protocol_or_error event >>| hello
        | Heartbeat_ack -> Ok Heartbeat_ack
        | Unknown _ -> Unknown event |> Ok
        | Identify
        | Presence_update
        | Voice_state_update
        | Resume
        | Request_guild_members
        | Request_soundboard_sounds ->
          Or_error.error_s
            [%message
              "Event with unsupported op code"
                (event : Websocket_protocol.Gateway.Event.t)]
      ;;
    end

    module Sendable = struct
      type t =
        | Heartbeat of Heartbeat.t
        | Identify of Identify.t
        | Resume of Resume.t
        | Voice_state_update of Voice_state_update.t
      [@@deriving sexp_of]

      let to_protocol = function
        | Heartbeat heartbeat -> Heartbeat.to_protocol heartbeat
        | Identify identify -> Identify.to_protocol identify
        | Resume resume -> Resume.to_protocol resume
        | Voice_state_update voice_state_update ->
          Voice_state_update.to_protocol voice_state_update
      ;;
    end
  end
end

module Voice_gateway = struct
  module Api_version = struct
    include Intable_extended.Make ()

    let v8 = of_int_exn 8

    let ws_url t ~base =
      base
      |> Fn.flip Uri.with_scheme (Some "wss")
      |> Fn.flip Uri.with_path "/"
      |> Fn.flip
           Uri.with_query
           [ "v", [ Int.to_string (to_int_exn t) ]; "encoding", [ "json" ] ]
    ;;
  end

  module Event = struct
    module Identify = struct
      type t =
        { server_id : Guild_id.t
        ; user_id : User_id.t
        ; session_id : Voice_gateway_session_id.t
        ; token : Voice_connection_token.t
        ; max_dave_protocol_version : int
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create ~data Identify
      ;;
    end

    module Ready = struct
      type t =
        { ssrc : Ssrc.t
        ; ip : string
        ; port : int
        ; modes : Tls_encryption_mode.t list
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Hello = struct
      type t = { heartbeat_interval : Time_ns.Span.t } [@@deriving sexp_of]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        match Yojson.Safe.Util.member "heartbeat_interval" data with
        | `Int heartbeat_interval ->
          Ok { heartbeat_interval = Time_ns.Span.of_int_ms heartbeat_interval }
        | data -> Or_error.error_s [%message "Invalid heartbeat_interval" (data : Json.t)]
      ;;
    end

    module Heartbeat = struct
      type t =
        { nonce : int [@key "t"]
        ; seq_ack : Websocket_protocol.Seq_num.t option [@default None]
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create ~data Heartbeat
      ;;
    end

    module Heartbeat_ack = struct
      type t = { nonce : string [@key "t"] }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Select_protocol = struct
      module Data = struct
        type t =
          { address : string
          ; port : int
          ; mode : Tls_encryption_mode.t
          }
        [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
      end

      type t =
        { protocol : string
        ; data : Data.t
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create ~data Select_protocol
      ;;
    end

    module Resume = struct
      type t =
        { server_id : Guild_id.t
        ; session_id : Voice_gateway_session_id.t
        ; token : Voice_connection_token.t
        ; seq_ack : Websocket_protocol.Seq_num.t option [@default None]
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create ~data Resume
      ;;
    end

    module Session_description = struct
      type t =
        { mode : Tls_encryption_mode.t
        ; secret_key : int array
        ; dave_protocol_version : int
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Speaking = struct
      type t =
        { speaking : int
        ; delay : int
        ; ssrc : Ssrc.t
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create ~data Speaking
      ;;
    end

    module Clients_connect = struct
      type t = { user_ids : string list }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Client_disconnect = struct
      type t = { user_id : string }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Dave_protocol_prepare_transition = struct
      type t =
        { transition_id : int
        ; protocol_version : int
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Dave_protocol_execute_transition = struct
      type t = { transition_id : int }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Dave_protocol_ready_for_transition = struct
      type t = { transition_id : int }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create
          ~data
          Dave_protocol_ready_for_transition
      ;;
    end

    module Dave_protocol_prepare_epoch = struct
      type t =
        { epoch : int
        ; protocol_version : int
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let of_protocol_or_error event =
        let%bind.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_json_or_error event
        in
        Or_error.try_with (fun () -> [%of_yojson: t] data)
      ;;
    end

    module Mls_external_sender_package = struct
      type t = { external_sender_package : string }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of]

      let of_protocol_or_error event =
        let%map.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_or_error event
        in
        { external_sender_package = Base64.encode_string data }
      ;;
    end

    module Mls_key_package = struct
      type t = { key_package : string } [@@yojson.allow_extra_fields] [@@deriving sexp_of]

      let to_protocol { key_package } =
        let data = Base64.decode_exn key_package in
        Websocket_protocol.Voice_gateway.Event.create ~data Mls_key_package
      ;;
    end

    module Mls_proposals = struct
      type t = { proposals : string } [@@yojson.allow_extra_fields] [@@deriving sexp_of]

      let of_protocol_or_error event =
        let%map.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_or_error event
        in
        { proposals = Base64.encode_string data }
      ;;
    end

    module Mls_commit_welcome = struct
      type t = { commit_welcome : string }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of]

      let to_protocol { commit_welcome } =
        let data = Base64.decode_exn commit_welcome in
        Websocket_protocol.Voice_gateway.Event.create ~data Mls_commit_welcome
      ;;
    end

    module Mls_announce_commit_transition = struct
      type t =
        { transition_id : int
        ; commit : string
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of]

      let of_protocol_or_error event =
        let%map.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_or_error event
        in
        let iobuf = Iobuf.of_string data in
        let transition_id = Iobuf.Consume.uint16_be iobuf in
        let commit = Iobuf.Consume.stringo iobuf |> Base64.encode_string in
        { transition_id; commit }
      ;;
    end

    module Mls_welcome = struct
      type t =
        { transition_id : int
        ; welcome : string
        }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of]

      let of_protocol_or_error event =
        let%map.Or_error data =
          Websocket_protocol.Voice_gateway.Event.data_or_error event
        in
        let iobuf = Iobuf.of_string data in
        let transition_id = Iobuf.Consume.uint16_be iobuf in
        let welcome = Iobuf.Consume.stringo iobuf |> Base64.encode_string in
        { transition_id; welcome }
      ;;
    end

    module Mls_invalid_commit_welcome = struct
      type t = { transition_id : int }
      [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]

      let to_protocol t =
        let data = [%yojson_of: t] t |> Json.to_string in
        Websocket_protocol.Voice_gateway.Event.create ~data Mls_invalid_commit_welcome
      ;;
    end

    module Receivable = struct
      type t =
        | Ready of Ready.t
        | Hello of Hello.t
        | Heartbeat_ack of Heartbeat_ack.t
        | Session_description of Session_description.t
        | Clients_connect of Clients_connect.t
        | Client_disconnect of Client_disconnect.t
        | Dave_protocol_prepare_transition of Dave_protocol_prepare_transition.t
        | Dave_protocol_execute_transition of Dave_protocol_execute_transition.t
        | Dave_protocol_prepare_epoch of Dave_protocol_prepare_epoch.t
        | Mls_external_sender_package of Mls_external_sender_package.t
        | Mls_proposals of Mls_proposals.t
        | Mls_announce_commit_transition of Mls_announce_commit_transition.t
        | Mls_welcome of Mls_welcome.t
        | Unknown of Websocket_protocol.Voice_gateway.Event.t
      [@@deriving sexp_of, variants]

      let of_protocol_or_error event =
        let open Or_error.Let_syntax in
        match Websocket_protocol.Voice_gateway.Event.op_code event with
        | Ready -> Ready.of_protocol_or_error event >>| ready
        | Hello -> Hello.of_protocol_or_error event >>| hello
        | Heartbeat_ack -> Heartbeat_ack.of_protocol_or_error event >>| heartbeat_ack
        | Session_description ->
          Session_description.of_protocol_or_error event >>| session_description
        | Clients_connect ->
          Clients_connect.of_protocol_or_error event >>| clients_connect
        | Client_disconnect ->
          Client_disconnect.of_protocol_or_error event >>| client_disconnect
        | Dave_protocol_prepare_transition ->
          Dave_protocol_prepare_transition.of_protocol_or_error event
          >>| dave_protocol_prepare_transition
        | Dave_protocol_execute_transition ->
          Dave_protocol_execute_transition.of_protocol_or_error event
          >>| dave_protocol_execute_transition
        | Dave_protocol_prepare_epoch ->
          Dave_protocol_prepare_epoch.of_protocol_or_error event
          >>| dave_protocol_prepare_epoch
        | Mls_external_sender_package ->
          Mls_external_sender_package.of_protocol_or_error event
          >>| mls_external_sender_package
        | Mls_proposals -> Mls_proposals.of_protocol_or_error event >>| mls_proposals
        | Mls_announce_commit_transition ->
          Mls_announce_commit_transition.of_protocol_or_error event
          >>| mls_announce_commit_transition
        | Mls_welcome -> Mls_welcome.of_protocol_or_error event >>| mls_welcome
        | Unknown _ -> Unknown event |> Ok
        | Identify
        | Heartbeat
        | Select_protocol
        | Speaking
        | Resume
        | Resumed
        | Dave_protocol_ready_for_transition
        | Mls_key_package
        | Mls_commit_welcome
        | Mls_invalid_commit_welcome ->
          Or_error.error_s
            [%message
              "Event with unsupported op code"
                (event : Websocket_protocol.Voice_gateway.Event.t)]
      ;;
    end

    module Sendable = struct
      type t =
        | Identify of Identify.t
        | Heartbeat of Heartbeat.t
        | Select_protocol of Select_protocol.t
        | Resume of Resume.t
        | Speaking of Speaking.t
        | Dave_protocol_ready_for_transition of Dave_protocol_ready_for_transition.t
        | Mls_key_package of Mls_key_package.t
        | Mls_commit_welcome of Mls_commit_welcome.t
        | Mls_invalid_commit_welcome of Mls_invalid_commit_welcome.t
      [@@deriving sexp_of]

      let to_protocol = function
        | Identify identify -> Identify.to_protocol identify
        | Heartbeat heartbeat -> Heartbeat.to_protocol heartbeat
        | Select_protocol select_protocol -> Select_protocol.to_protocol select_protocol
        | Resume resume -> Resume.to_protocol resume
        | Speaking speaking -> Speaking.to_protocol speaking
        | Dave_protocol_ready_for_transition ready ->
          Dave_protocol_ready_for_transition.to_protocol ready
        | Mls_key_package key_package -> Mls_key_package.to_protocol key_package
        | Mls_commit_welcome commit_welcome ->
          Mls_commit_welcome.to_protocol commit_welcome
        | Mls_invalid_commit_welcome invalid ->
          Mls_invalid_commit_welcome.to_protocol invalid
      ;;
    end
  end
end
