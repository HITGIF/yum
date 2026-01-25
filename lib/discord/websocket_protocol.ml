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

    let frame_content = Fn.const `Text

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
      | Mls_announce_commit_transition
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
      | Mls_announce_commit_transition -> 29
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
      | 29 -> Mls_announce_commit_transition
      | 30 -> Mls_welcome
      | 31 -> Mls_invalid_commit_welcome
      | n -> Unknown n
    ;;

    let frame_content = function
      | Mls_key_package | Mls_commit_welcome -> `Binary
      | _ -> `Text
    ;;

    include functor Make
  end
end

module Event = struct
  module Make
      (Op_code : sig
         type t [@@deriving equal, sexp_of, quickcheck, yojson]

         val of_int : int -> t
         val to_int : t -> int
         val frame_content : t -> [ `Text | `Binary ]
       end)
      (Arg : sig
         type t [@@deriving sexp_of, yojson]

         val op_code : t -> Op_code.t
         val data : t -> string option
         val seq_num : t -> Seq_num.t option
         val name : t -> string option

         module Fields : sig
           val create
             :  op_code:Op_code.t
             -> data:string option
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

    let data_json_or_error t =
      let%bind.Or_error data = data_or_error t in
      Or_error.try_with (fun () -> Json.from_string data |> [%of_yojson: Json.t])
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

    let of_frame_content_or_error { Websocket.Frame_content.opcode; content } =
      match opcode with
      | Text ->
        Or_error.try_with (fun () -> Json.from_string content |> [%of_yojson: Arg.t])
      | Binary ->
        (* Binary DAVE MLS messages:
           - uint16: sequence number (big-endian)
           - uint8: opcode
           - rest: payload *)
        Or_error.try_with (fun () ->
          let iobuf = Iobuf.of_string content in
          let seq_num = Iobuf.Consume.uint16_be iobuf |> Seq_num.of_int_exn in
          let op_code = Iobuf.Consume.uint8 iobuf |> Op_code.of_int in
          let data = Iobuf.Consume.stringo iobuf in
          Arg.Fields.create ~op_code ~data:(Some data) ~seq_num:(Some seq_num) ~name:None)
      | Nonctrl _ as opcode ->
        Or_error.error_s
          [%message
            "Unexpected Websocket frame content opcode"
              (opcode : Websocket.Frame_content.Opcode.t)]
    ;;

    let to_frame_content t =
      match Arg.op_code t |> Op_code.frame_content with
      | `Text ->
        { Websocket.Frame_content.opcode = Text
        ; content = [%yojson_of: Arg.t] t |> Json.to_string
        }
      | `Binary ->
        let payload = Arg.data t |> Option.value ~default:"" in
        let iobuf = Iobuf.create ~len:(String.length payload + 1) in
        Iobuf.Fill.uint8_trunc iobuf (Arg.op_code t |> Op_code.to_int);
        Iobuf.Fill.stringo iobuf payload;
        Iobuf.flip_lo iobuf;
        let content = Iobuf.to_string iobuf in
        { Websocket.Frame_content.opcode = Binary; content }
    ;;
  end

  module Data = struct
    type t = string [@@deriving sexp_of]

    let t_of_yojson = Json.to_string
    let yojson_of_t = Json.from_string
  end

  module Gateway = struct
    type t =
      { op_code : Op_code.Gateway.t [@key "op"]
      ; data : Data.t option [@key "d"] [@default None]
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
      ; data : Data.t option [@key "d"] [@default None]
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
