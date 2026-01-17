open! Core
open! Common
open! Discord
open! Expect_test_helpers_core

let%expect_test "Gateway API URL" =
  Model.Gateway.Api_version.(rest_url v10) |> Model.Uri.to_string |> print_endline;
  [%expect {| https://discord.com/api/v10 |}];
  Model.Gateway.Api_version.(
    ws_url v10 ~base:(Model.Uri.of_string "wss://gateway.discord.gg"))
  |> Model.Uri.to_string
  |> print_endline;
  [%expect {| wss://gateway.discord.gg/?v=10&encoding=json |}]
;;

let%expect_test "Voice Gateway API URL" =
  Model.Voice_gateway.Api_version.(
    ws_url v8 ~base:(Model.Uri.of_string "wss://sweetwater-12345.discord.media:2048"))
  |> Model.Uri.to_string
  |> print_endline;
  [%expect {| wss://sweetwater-12345.discord.media:2048/?v=8&encoding=json |}]
;;

let test_receive payload =
  payload
  |> Json.from_string
  |> [%of_yojson: Websocket_protocol.Gateway.Event.t]
  |> Model.Gateway.Event.Receivable.of_protocol_or_error
  |> [%sexp_of: Model.Gateway.Event.Receivable.t Or_error.t]
  |> print_s
;;

let test_voice_gateway_receive payload =
  payload
  |> Json.from_string
  |> [%of_yojson: Websocket_protocol.Voice_gateway.Event.t]
  |> Model.Voice_gateway.Event.Receivable.of_protocol_or_error
  |> [%sexp_of: Model.Voice_gateway.Event.Receivable.t Or_error.t]
  |> print_s
;;

let test_send event =
  event
  |> Model.Gateway.Event.Sendable.to_protocol
  |> [%yojson_of: Websocket_protocol.Gateway.Event.t]
  |> Json.pretty_to_string
  |> print_endline
;;

let test_roundtrip event =
  event
  |> Model.Gateway.Event.Sendable.to_protocol
  |> [%yojson_of: Websocket_protocol.Gateway.Event.t]
  |> fun json ->
  Json.pretty_to_string json |> print_endline;
  json
  |> Json.to_string
  |> Json.from_string
  |> [%of_yojson: Websocket_protocol.Gateway.Event.t]
  |> Model.Gateway.Event.Receivable.of_protocol_or_error
  |> [%sexp_of: Model.Gateway.Event.Receivable.t Or_error.t]
  |> print_s
;;

let%expect_test "hello" =
  test_receive
    {|
    {
      "op": 10,
      "d": {
        "heartbeat_interval": 45000
      }
    }
    |};
  [%expect {| (Ok (Hello ((heartbeat_interval 45s)))) |}];
  test_receive
    {|
    {
      "t": null,
      "s": null,
      "op": 10,
      "d": {
        "heartbeat_interval": 41250,
        "_trace": [
          "[\"gateway-prd-arm-us-east1-c-tnh2\",{\"micros\":0.0}]"
        ]
      }
    }
    |};
  [%expect {| (Ok (Hello ((heartbeat_interval 41.25s)))) |}]
;;

let%expect_test "heartbeat" =
  test_roundtrip (Heartbeat { last_seq_num = None });
  [%expect {|
    { "op": 1, "d": null, "s": null, "t": null }
    (Ok Heartbeat)
    |}];
  test_roundtrip
    (Heartbeat { last_seq_num = Some (Websocket_protocol.Seq_num.of_int_exn 42) });
  [%expect {|
    { "op": 1, "d": 42, "s": null, "t": null }
    (Ok Heartbeat)
    |}]
;;

let%expect_test "identify" =
  test_send
    (Identify
       { token = Model.Auth_token.of_string "my_token"
       ; intents =
           Model.Intents.create
             [ Guilds; Guild_voice_states; Guild_messages; Message_content ]
       ; properties = { os = "Linux"; browser = "my_library"; device = "my_library" }
       });
  [%expect
    {|
    {
      "op": 2,
      "d": {
        "token": "my_token",
        "intents": 33409,
        "properties": {
          "os": "Linux",
          "browser": "my_library",
          "device": "my_library"
        }
      },
      "s": null,
      "t": null
    }
    |}]
;;

let%expect_test "ready" =
  test_receive
    {|
    {
      "t": "READY",
      "s": 1,
      "op": 0,
      "d": {
        "v": 10,
        "user_settings": {},
        "user": {
          "verified": true,
          "username": "yum",
          "primary_guild": null,
          "mfa_enabled": false,
          "id": "1178012787457400832",
          "global_name": null,
          "flags": 0,
          "email": null,
          "discriminator": "2729",
          "clan": null,
          "bot": true,
          "avatar": "43fe17b2ea62ee551eebc94e7ed04943"
        },
        "session_type": "normal",
        "session_id": "7eca52a96fa851e0183b0592107f7a71",
        "resume_gateway_url": "wss://gateway-us-east1-c.discord.gg",
        "relationships": [],
        "private_channels": [],
        "presences": [],
        "guilds": [
          {
            "unavailable": true,
            "id": "641810999552245793"
          },
          {
            "unavailable": true,
            "id": "737305786119880804"
          },
          {
            "unavailable": true,
            "id": "923993216158687232"
          },
          {
            "unavailable": true,
            "id": "974140131869085706"
          },
          {
            "unavailable": true,
            "id": "1067612240246743100"
          },
          {
            "unavailable": true,
            "id": "1193791627857244240"
          }
        ],
        "guild_join_requests": [],
        "geo_ordered_rtc_regions": [
          "hongkong",
          "singapore",
          "japan",
          "india",
          "dubai"
        ],
        "game_relationships": [],
        "auth": {},
        "application": {
          "id": "1178012787457400832",
          "flags": 565248
        },
        "_trace": [
          "[\"gateway-prd-arm-us-east1-c-tnh2\",{\"micros\":260632,\"calls\":[\"id_created\",{\"micros\":639,\"calls\":[]},\"session_lookup_time\",{\"micros\":236,\"calls\":[]},\"session_lookup_finished\",{\"micros\":8,\"calls\":[]},\"discord-sessions-prd-2-143\",{\"micros\":259468,\"calls\":[\"start_session\",{\"micros\":233791,\"calls\":[\"discord-api-rpc-685f5fd59d-rxk9t\",{\"micros\":189680,\"calls\":[\"get_user\",{\"micros\":23727},\"get_guilds\",{\"micros\":94934},\"send_scheduled_deletion_message\",{\"micros\":17},\"guild_join_requests\",{\"micros\":2},\"authorized_ip_coro\",{\"micros\":13},\"pending_payments\",{\"micros\":597},\"apex_experiments\",{\"micros\":36642},\"user_activities\",{\"micros\":5},\"played_application_ids\",{\"micros\":3},\"linked_users\",{\"micros\":3}]}]},\"starting_guild_connect\",{\"micros\":69,\"calls\":[]},\"presence_started\",{\"micros\":322,\"calls\":[]},\"guilds_started\",{\"micros\":112,\"calls\":[]},\"lobbies_started\",{\"micros\":1,\"calls\":[]},\"guilds_connect\",{\"micros\":1,\"calls\":[]},\"presence_connect\",{\"micros\":25147,\"calls\":[]},\"connect_finished\",{\"micros\":25155,\"calls\":[]},\"build_ready\",{\"micros\":15,\"calls\":[]},\"clean_ready\",{\"micros\":1,\"calls\":[]},\"optimize_ready\",{\"micros\":0,\"calls\":[]},\"split_ready\",{\"micros\":1,\"calls\":[]}]}]}]"
        ]
      }
    }
    |};
  [%expect
    {|
    (Ok (
      Dispatch (
        Ready (
          (v 10)
          (user (
            (id       1178012787457400832)
            (username yum)
            (global_name ())
            (bot (true))))
          (guilds (
            (Assoc (
              (unavailable (Bool   true))
              (id          (String 641810999552245793))))
            (Assoc (
              (unavailable (Bool   true))
              (id          (String 737305786119880804))))
            (Assoc (
              (unavailable (Bool   true))
              (id          (String 923993216158687232))))
            (Assoc (
              (unavailable (Bool   true))
              (id          (String 974140131869085706))))
            (Assoc (
              (unavailable (Bool   true))
              (id          (String 1067612240246743100))))
            (Assoc (
              (unavailable (Bool   true))
              (id          (String 1193791627857244240))))))
          (session_id 7eca52a96fa851e0183b0592107f7a71)
          (resume_gateway_url wss://gateway-us-east1-c.discord.gg)
          (application (
            Assoc (
              (id    (String 1178012787457400832))
              (flags (Int    565248)))))))))
    |}]
;;

let%expect_test "guild create" =
  test_receive
    {|
    {
      "t": "GUILD_CREATE",
      "s": 5,
      "op": 0,
      "d": {
        "presences": [],
        "premium_features": null,
        "activity_instances": [],
        "inventory_settings": null,
        "guild_scheduled_events": [],
        "roles": [
          {
            "version": 1704692503703,
            "unicode_emoji": null,
            "tags": {},
            "position": 0,
            "permissions": "2248473465835073",
            "name": "@everyone",
            "mentionable": false,
            "managed": false,
            "id": "1193791627857244240",
            "icon": null,
            "hoist": false,
            "flags": 0,
            "colors": {
              "tertiary_color": null,
              "secondary_color": null,
              "primary_color": 0
            },
            "color": 0
          },
          {
            "version": 1704693335367,
            "unicode_emoji": null,
            "tags": {
              "bot_id": "1178012787457400832"
            },
            "position": 1,
            "permissions": "2150631424",
            "name": "yum",
            "mentionable": false,
            "managed": true,
            "id": "1193795115878195214",
            "icon": null,
            "hoist": false,
            "flags": 0,
            "colors": {
              "tertiary_color": null,
              "secondary_color": null,
              "primary_color": 0
            },
            "color": 0
          }
        ],
        "profile": null,
        "nsfw": false,
        "features": [],
        "emojis": [],
        "lazy": true,
        "incidents_data": null,
        "id": "1193791627857244240",
        "voice_states": [
          {
            "channel_id": "157733188964188161",
            "user_id": "80351110224678912",
            "session_id": "90326bd25d71d39b9ef95b299e3872ff",
            "deaf": false,
            "mute": false,
            "self_deaf": false,
            "self_mute": true,
            "suppress": false,
            "request_to_speak_timestamp": "2021-03-31T18:45:31.297561+00:00"
          }
        ],
        "default_message_notifications": 0,
        "name": "444",
        "mfa_level": 0,
        "system_channel_id": null,
        "threads": [],
        "large": false,
        "afk_timeout": 300,
        "owner_id": "504867623150813186",
        "premium_tier": 0,
        "hub_type": null,
        "members": [
          {
            "user": {
              "username": "yum",
              "public_flags": 0,
              "primary_guild": null,
              "id": "1178012787457400832",
              "global_name": null,
              "display_name_styles": null,
              "display_name": null,
              "discriminator": "2729",
              "collectibles": null,
              "bot": true,
              "avatar_decoration_data": null,
              "avatar": "43fe17b2ea62ee551eebc94e7ed04943"
            },
            "roles": [
              "1193795115878195214"
            ],
            "premium_since": null,
            "pending": false,
            "nick": null,
            "mute": false,
            "joined_at": "2024-01-08T05:55:35.380000+00:00",
            "flags": 0,
            "deaf": false,
            "communication_disabled_until": null,
            "banner": null,
            "avatar": null
          }
        ],
        "moderator_reporting": null,
        "max_members": 25000000,
        "latest_onboarding_question_id": null,
        "premium_subscription_count": 0,
        "description": null,
        "joined_at": "2024-01-08T05:55:35.380000+00:00",
        "vanity_url_code": null,
        "system_channel_flags": 0,
        "owner_configured_content_level": 0,
        "banner": null,
        "max_video_channel_users": 25,
        "afk_channel_id": null,
        "max_stage_video_channel_users": 50,
        "region": "deprecated",
        "application_id": null,
        "discovery_splash": null,
        "preferred_locale": "en-US",
        "stage_instances": [],
        "application_command_counts": {},
        "premium_progress_bar_enabled": false,
        "unavailable": false,
        "icon": "51b9825fb78154ed6d0611757c29a782",
        "embedded_activities": [],
        "explicit_content_filter": 0,
        "rules_channel_id": null,
        "member_count": 4,
        "verification_level": 0,
        "home_header": null,
        "public_updates_channel_id": null,
        "soundboard_sounds": [],
        "version": 1704733247156,
        "nsfw_level": 0,
        "channels": [
          {
            "version": 1704692503705,
            "type": 4,
            "position": 0,
            "permission_overwrites": [],
            "name": "Text Channels",
            "id": "1193791627857244241",
            "flags": 0
          },
          {
            "version": 1704692503708,
            "type": 4,
            "position": 0,
            "permission_overwrites": [],
            "name": "Voice Channels",
            "id": "1193791627857244242",
            "flags": 0
          },
          {
            "version": 1705942372766,
            "type": 0,
            "topic": null,
            "rate_limit_per_user": 0,
            "position": 0,
            "permission_overwrites": [],
            "parent_id": "1193791627857244241",
            "name": "general",
            "last_pin_timestamp": "2024-01-22T16:52:52+00:00",
            "last_message_id": "1233611595188535296",
            "id": "1193791627857244243",
            "icon_emoji": {
              "name": "👋",
              "id": null
            },
            "flags": 0
          },
          {
            "version": 1704798392869,
            "user_limit": 0,
            "type": 2,
            "rtc_region": null,
            "rate_limit_per_user": 0,
            "position": 0,
            "permission_overwrites": [],
            "parent_id": "1193791627857244242",
            "name": "😋",
            "last_message_id": "1214031192370839592",
            "id": "1193791627857244244",
            "icon_emoji": {
              "name": "🎙️",
              "id": null
            },
            "flags": 0,
            "bitrate": 96000
          }
        ],
        "stickers": [],
        "safety_alerts_channel_id": null,
        "splash": null
      }
    }
    |};
  [%expect
    {|
    (Ok (
      Dispatch (
        Guild_create (
          (id 1193791627857244240)
          (name        (444))
          (unavailable (false))
          (voice_states ((
            (guild_id ())
            (channel_id (157733188964188161))
            (user_id    80351110224678912)
            (session_id 90326bd25d71d39b9ef95b299e3872ff))))))))
    |}]
;;

let%expect_test "unknown dispatch" =
  test_receive
    {|
    {
      "t": "FOO",
      "s": 5,
      "op": 0,
      "d": {}
    }
    |};
  [%expect {| (Ok (Dispatch (Unknown (name FOO) (data (Assoc ()))))) |}]
;;

let%expect_test "invalid session" =
  test_receive {|
    {
      "op": 9,
      "d": false
    }
    |};
  [%expect {| (Ok (Invalid_session ((resumable false)))) |}];
  test_receive {|
    {
      "op": 9,
      "d": true
    }
    |};
  [%expect {| (Ok (Invalid_session ((resumable true)))) |}];
  test_receive {|
    {
      "op": 9,
      "d": null
    }
    |};
  [%expect {| (Ok (Invalid_session ((resumable false)))) |}];
  test_receive {|
    {
      "op": 9
    }
    |};
  [%expect {| (Ok (Invalid_session ((resumable false)))) |}]
;;

let%expect_test "resume" =
  test_send
    (Resume
       { token = Model.Auth_token.of_string "my_token"
       ; session_id = Model.Gateway_session_id.of_string "my_session"
       ; seq = Some (Websocket_protocol.Seq_num.of_int_exn 123)
       });
  [%expect
    {|
    {
      "op": 6,
      "d": { "token": "my_token", "session_id": "my_session", "seq": 123 },
      "s": null,
      "t": null
    }
    |}];
  test_send
    (Resume
       { token = Model.Auth_token.of_string "my_token"
       ; session_id = Model.Gateway_session_id.of_string "my_session"
       ; seq = None
       });
  [%expect
    {|
    {
      "op": 6,
      "d": { "token": "my_token", "session_id": "my_session", "seq": null },
      "s": null,
      "t": null
    }
    |}]
;;

let%expect_test "resumed" =
  test_receive
    {|
    {
      "t": "RESUMED",
      "op": 0,
      "d": {
        "_trace": [
          "[\"gateway-prd-arm-us-east1-c-tnh2\",{\"micros\":260632,\"calls\":[\"id_created\",{\"micros\":639,\"calls\":[]},\"session_lookup_time\",{\"micros\":236,\"calls\":[]},\"session_lookup_finished\",{\"micros\":8,\"calls\":[]},\"discord-sessions-prd-2-143\",{\"micros\":259468,\"calls\":[\"start_session\",{\"micros\":233791,\"calls\":[\"discord-api-rpc-685f5fd59d-rxk9t\",{\"micros\":189680,\"calls\":[\"get_user\",{\"micros\":23727},\"get_guilds\",{\"micros\":94934},\"send_scheduled_deletion_message\",{\"micros\":17},\"guild_join_requests\",{\"micros\":2},\"authorized_ip_coro\",{\"micros\":13},\"pending_payments\",{\"micros\":597},\"apex_experiments\",{\"micros\":36642},\"user_activities\",{\"micros\":5},\"played_application_ids\",{\"micros\":3},\"linked_users\",{\"micros\":3}]}]},\"starting_guild_connect\",{\"micros\":69,\"calls\":[]},\"presence_started\",{\"micros\":322,\"calls\":[]},\"guilds_started\",{\"micros\":112,\"calls\":[]},\"lobbies_started\",{\"micros\":1,\"calls\":[]},\"guilds_connect\",{\"micros\":1,\"calls\":[]},\"presence_connect\",{\"micros\":25147,\"calls\":[]},\"connect_finished\",{\"micros\":25155,\"calls\":[]},\"build_ready\",{\"micros\":15,\"calls\":[]},\"clean_ready\",{\"micros\":1,\"calls\":[]},\"optimize_ready\",{\"micros\":0,\"calls\":[]},\"split_ready\",{\"micros\":1,\"calls\":[]}]}]}]"
        ]
      }
    }
    |};
  [%expect {| (Ok (Dispatch Resumed)) |}]
;;

let%expect_test "Voice_server_update" =
  test_receive
    {|
    {
      "t": "VOICE_SERVER_UPDATE",
      "op": 0,
      "d": {
        "token": "my_token",
        "guild_id": "41771983423143937",
        "endpoint": "sweetwater-12345.discord.media:2048"
      }
    }
    |};
  [%expect
    {|
    (Ok (
      Dispatch (
        Voice_server_update (
          (token    my_token)
          (guild_id 41771983423143937)
          (endpoint (wss://sweetwater-12345.discord.media:2048))))))
    |}]
;;

let%expect_test "voice gateway heartbeat ack" =
  test_voice_gateway_receive {|
    {"op":6,"d":{"t":"1767537512688773475"}}
    |};
  [%expect {| (Ok (Heartbeat_ack ((nonce 1767537512688773475)))) |}]
;;
