open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
include Websocket_protocol_intf

module Op_code = struct
  module Unknown = struct
    type t = int [@@deriving equal, sexp_of, quickcheck]

    let quickcheck_generator = Quickcheck.Generator.of_list [ 111; 222; 333 ]
  end

  module Make (Arg : sig
      type t [@@deriving equal, sexp_of, quickcheck]

      val of_int : int -> t
      val to_int : t -> int
    end) =
  struct
    let%quick_test "int roundtrip" =
      fun (t : Arg.t) ->
      let t' = t |> Arg.to_int |> Arg.of_int in
      assert (Arg.equal t t');
      [%expect {| |}]
    ;;

    let yojson_of_t t = Arg.to_int t |> [%yojson_of: int]
    let t_of_yojson json = json |> [%of_yojson: int] |> Arg.of_int
  end

  module Gateway = struct
    type t = Op_code.Gateway.t =
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
      | Unknown of Unknown.t
    [@@deriving equal, sexp_of, quickcheck]

    let to_int = function
      | Dispatch -> 0
      | Heartbeat -> 1
      | Identify -> 2
      | Presence_update -> 3
      | Voice_state_update -> 4
      | Resume -> 6
      | Reconnect -> 7
      | Request_guild_members -> 8
      | Invalid_session -> 9
      | Hello -> 10
      | Heartbeat_ack -> 11
      | Request_soundboard_sounds -> 31
      | Unknown n -> n
    ;;

    let of_int = function
      | 0 -> Dispatch
      | 1 -> Heartbeat
      | 2 -> Identify
      | 3 -> Presence_update
      | 4 -> Voice_state_update
      | 6 -> Resume
      | 7 -> Reconnect
      | 8 -> Request_guild_members
      | 9 -> Invalid_session
      | 10 -> Hello
      | 11 -> Heartbeat_ack
      | 31 -> Request_soundboard_sounds
      | n -> Unknown n
    ;;

    include functor Make
  end

  module Voice_gateway = struct
    type t = Op_code.Voice_gateway.t =
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
      | Mls_prepare_commit_transition
      | Mls_welcome
      | Mls_invalid_commit_welcome
      | Unknown of Unknown.t
    [@@deriving equal, sexp_of, quickcheck]

    let to_int = function
      | Identify -> 0
      | Select_protocol -> 1
      | Ready -> 2
      | Heartbeat -> 3
      | Session_description -> 4
      | Speaking -> 5
      | Heartbeat_ack -> 6
      | Resume -> 7
      | Hello -> 8
      | Resumed -> 9
      | Clients_connect -> 11
      | Client_disconnect -> 13
      | Dave_protocol_prepare_transition -> 21
      | Dave_protocol_execute_transition -> 22
      | Dave_protocol_ready_for_transition -> 23
      | Dave_protocol_prepare_epoch -> 24
      | Mls_external_sender_package -> 25
      | Mls_key_package -> 26
      | Mls_proposals -> 27
      | Mls_commit_welcome -> 28
      | Mls_prepare_commit_transition -> 29
      | Mls_welcome -> 30
      | Mls_invalid_commit_welcome -> 31
      | Unknown n -> n
    ;;

    let of_int = function
      | 0 -> Identify
      | 1 -> Select_protocol
      | 2 -> Ready
      | 3 -> Heartbeat
      | 4 -> Session_description
      | 5 -> Speaking
      | 6 -> Heartbeat_ack
      | 7 -> Resume
      | 8 -> Hello
      | 9 -> Resumed
      | 11 -> Clients_connect
      | 13 -> Client_disconnect
      | 21 -> Dave_protocol_prepare_transition
      | 22 -> Dave_protocol_execute_transition
      | 23 -> Dave_protocol_ready_for_transition
      | 24 -> Dave_protocol_prepare_epoch
      | 25 -> Mls_external_sender_package
      | 26 -> Mls_key_package
      | 27 -> Mls_proposals
      | 28 -> Mls_commit_welcome
      | 29 -> Mls_prepare_commit_transition
      | 30 -> Mls_welcome
      | 31 -> Mls_invalid_commit_welcome
      | n -> Unknown n
    ;;

    include functor Make
  end
end

module Event = struct
  module Make
      (Op_code : sig
         type t [@@deriving equal, sexp_of, quickcheck, yojson]
       end)
      (Arg : sig
         type t [@@deriving sexp_of, yojson]

         val data : t -> Json.t option
         val seq_num : t -> Seq_num.t option
         val name : t -> string option

         module Fields : sig
           val create
             :  op_code:Op_code.t
             -> data:Json.t option
             -> seq_num:Seq_num.t option
             -> name:string option
             -> t
         end
       end) =
  struct
    let create ?data ?name op_code = Arg.Fields.create ~op_code ~data ~seq_num:None ~name

    let data_or_error t =
      match Arg.data t with
      | Some data -> Ok data
      | None -> Or_error.error_s [%message "Missing data" (t : Arg.t)]
    ;;

    let seq_num_or_error t =
      match Arg.seq_num t with
      | Some seq_num -> Ok seq_num
      | None -> Or_error.error_s [%message "Missing seq_num" (t : Arg.t)]
    ;;

    let name_or_error t =
      match Arg.name t with
      | Some name -> Ok name
      | None -> Or_error.error_s [%message "Missing name" (t : Arg.t)]
    ;;
  end

  module Gateway = struct
    type t =
      { op_code : Op_code.Gateway.t [@key "op"]
      ; data : Json.t option [@key "d"] [@default None]
      ; seq_num : Seq_num.t option [@key "s"] [@default None]
      ; name : string option [@key "t"] [@default None]
      }
    [@@yojson.allow_extra_fields]
    [@@deriving fields ~getters ~iterators:create, sexp_of, yojson]

    include functor Make (Op_code.Gateway)
  end

  module Voice_gateway = struct
    type t =
      { op_code : Op_code.Voice_gateway.t [@key "op"]
      ; data : Json.t option [@key "d"] [@default None]
      ; seq_num : Seq_num.t option [@key "seq"] [@default None]
      ; name : string option [@key "t"] [@default None]
      }
    [@@yojson.allow_extra_fields]
    [@@deriving fields ~getters ~iterators:create, sexp_of, yojson]

    include functor Make (Op_code.Voice_gateway)
  end
end

module Gateway = struct
  module Op_code = Op_code.Gateway
  module Event = Event.Gateway
end

module Voice_gateway = struct
  module Op_code = Op_code.Voice_gateway
  module Event = Event.Voice_gateway
end
