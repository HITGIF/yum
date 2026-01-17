open! Core
open! Async
module Intable_extended = Common.Intable_extended
module Json = Common.Json

module Websocket = struct
  include Websocket

  let reader t = pipes t |> fst
  let writer t = pipes t |> snd
end

module Event = struct
  type t =
    | Voice_ready of
        { channel_id : Model.Channel_id.t
        ; frames_writer : Audio.Pcm_frame.t Queue.t Pipe.Writer.t
        }
end

module State = struct
  module Connected = struct
    module State = struct
      type t =
        | Waiting_for_ready
        | Waiting_for_session_description of { voice_udp : (Voice_udp.t[@sexp.opaque]) }
        | Ready_to_send of
            { frames_writer : (Audio.Pcm_frame.t Base.Queue.t Pipe.Writer.t[@sexp.opaque])
            }
      [@@deriving sexp_of]
    end

    type t =
      { mutable state : State.t
      ; mutable last_seq_num : Websocket_protocol.Seq_num.t option
      ; mutable last_heartbeat_acked : bool
      ; heartbeating : unit Set_once.t
      ; created_at : Time_ns.t
      ; ws : (string Websocket.t[@sexp.opaque])
      }
    [@@deriving sexp_of]

    let create ~time_source state ws =
      let created_at = Time_source.now time_source in
      { ws
      ; created_at
      ; state
      ; last_seq_num = None
      ; last_heartbeat_acked = true
      ; heartbeating = Set_once.create ()
      }
    ;;
  end

  type t =
    | Disconnected
    | Connected of Connected.t
  [@@deriving sexp_of]
end

type t =
  { mutable state : State.t
  ; time_source : Time_source.t
  ; token : Model.Voice_connection_token.t
  ; endpoint : Model.Uri.t
  ; guild_id : Model.Guild_id.t
  ; channel_id : Model.Channel_id.t
  ; session_id : Model.Voice_gateway_session_id.t
  ; user_id : Model.User_id.t
  ; reincarnate : unit -> unit Deferred.t
  ; events_reader : Event.t Pipe.Reader.t
  ; events_writer : Event.t Pipe.Writer.t
  }
[@@deriving fields ~getters]

let create
  ?time_source
  ~token
  ~endpoint
  ~guild_id
  ~channel_id
  ~session_id
  ~user_id
  ~reincarnate
  ()
  =
  let time_source =
    match time_source with
    | None -> Time_source.wall_clock ()
    | Some time_source -> Time_source.read_only time_source
  in
  let events_reader, events_writer = Pipe.create () in
  { state = Disconnected
  ; time_source
  ; token
  ; endpoint
  ; guild_id
  ; channel_id
  ; session_id
  ; user_id
  ; reincarnate
  ; events_reader
  ; events_writer
  }
;;

let events t = t.events_reader
let emit t event = Pipe.write_without_pushback_if_open t.events_writer event

let write_if_connected t event =
  match t.state with
  | Disconnected ->
    [%log.error
      [%here]
        "Ignoring write when disconnected"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (event : Model.Voice_gateway.Event.Sendable.t)];
    return ()
  | Connected connected ->
    (match%map
       Deferred.Or_error.try_with_join (fun () ->
         event
         |> Model.Voice_gateway.Event.Sendable.to_protocol
         |> [%yojson_of: Websocket_protocol.Voice_gateway.Event.t]
         |> Json.to_string
         |> Pipe.write_if_open (Websocket.writer connected.ws)
         |> Deferred.ok)
     with
     | Ok () ->
       [%log.debug
         [%here]
           "Sent"
           ~guild_id:(t.guild_id : Model.Guild_id.t)
           (event : Model.Voice_gateway.Event.Sendable.t)]
     | Error error ->
       [%log.error
         "Failed to write voice gateway event"
           ~guild_id:(t.guild_id : Model.Guild_id.t)
           (event : Model.Voice_gateway.Event.Sendable.t)
           (error : Error.t)])
;;

let send_heartbeat t last_seq_num =
  let nonce = Time_source.now t.time_source |> Time_ns.to_int_ns_since_epoch in
  write_if_connected t (Heartbeat { nonce; seq_ack = last_seq_num })
;;

let send_identify t =
  write_if_connected
    t
    (Identify
       { token = t.token
       ; server_id = t.guild_id
       ; session_id = t.session_id
       ; user_id = t.user_id
       ; max_dave_protocol_version = 0 (* CR-someday: DAVE support *)
       })
;;

let send_speaking t speaking = write_if_connected t (Speaking speaking)

let formulate_url base =
  Model.Voice_gateway.Api_version.(ws_url v8 ~base) |> Model.Uri.to_uri
;;

let disconnect t =
  match t.state with
  | Disconnected ->
    [%log.debug [%here] "Already disconnected" ~guild_id:(t.guild_id : Model.Guild_id.t)];
    return ()
  | Connected
      { ws
      ; state = _
      ; last_seq_num = _
      ; last_heartbeat_acked = _
      ; heartbeating = _
      ; created_at = _
      } ->
    [%log.info [%here] "Disconnecting..." ~guild_id:(t.guild_id : Model.Guild_id.t)];
    let reader = Websocket.reader ws in
    Pipe.close_read reader;
    let%bind _ = Websocket.close_finished ws in
    t.state <- Disconnected;
    [%log.info [%here] "Disconnected" ~guild_id:(t.guild_id : Model.Guild_id.t)];
    return ()
;;

let close_voice_udp t =
  match t.state with
  | Connected { state = Ready_to_send { frames_writer }; _ } ->
    Pipe.close frames_writer;
    Pipe.closed frames_writer
  | _ -> return ()
;;

let reincarnate t =
  [%log.info [%here] "Reincarnating..." ~guild_id:(t.guild_id : Model.Guild_id.t)];
  let%bind () = close_voice_udp t in
  let%bind () = disconnect t in
  t.reincarnate ()
;;

let handle_ready t { Model.Voice_gateway.Event.Ready.ssrc; ip; port; modes } =
  match t.state with
  | Connected ({ state = Waiting_for_ready; _ } as connected) ->
    [%log.info
      [%here]
        "Connecting voice UDP"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (ip : string)
        (port : int)];
    let%bind voice_udp =
      Voice_udp.create
        ~ssrc
        ~ip
        ~port
        ~encryption_modes:modes
        ~send_speaking:(send_speaking t)
      >>| Or_error.ok_exn
    in
    (match%bind Voice_udp.discover_ip voice_udp with
     | Error error ->
       [%log.error
         [%here]
           "Failed to discover IP and Port, reincarnating voice gateway"
           ~guild_id:(t.guild_id : Model.Guild_id.t)
           (error : Error.t)];
       reincarnate t
     | Ok (~my_ip, ~my_port) ->
       [%log.debug
         [%here]
           "Discovered IP and Port"
           ~guild_id:(t.guild_id : Model.Guild_id.t)
           (my_ip : string)
           (my_port : int)];
       let%bind () =
         write_if_connected
           t
           (Select_protocol
              { protocol = "udp"
              ; data =
                  { address = my_ip
                  ; port = my_port
                  ; mode = Voice_udp.encryption_mode voice_udp
                  }
              })
       in
       connected.state <- Waiting_for_session_description { voice_udp };
       return ())
  | state ->
    [%log.error
      [%here]
        "Ignoring [Ready] in unexpected state"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (state : State.t)];
    return ()
;;

let handle_session_description
  t
  { Model.Voice_gateway.Event.Session_description.secret_key
  ; mode = _
  ; dave_protocol_version = _
  }
  =
  match t.state with
  | Connected ({ state = Waiting_for_session_description { voice_udp }; _ } as connected)
    ->
    let secret_key =
      List.of_array secret_key |> List.map ~f:Char.of_int_exn |> Bytes.of_char_list
    in
    let frames_writer = Voice_udp.frames_writer voice_udp ~secret_key in
    connected.state <- Ready_to_send { frames_writer };
    [%log.debug [%here] "Ready to send" ~guild_id:(t.guild_id : Model.Guild_id.t)];
    emit t (Voice_ready { channel_id = t.channel_id; frames_writer })
  | state ->
    [%log.error
      [%here]
        "Ignoring [Session_description] in unexpected state"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (state : State.t)]
;;

let handle_heartbeat_ack t =
  match t.state with
  | Connected connected -> connected.last_heartbeat_acked <- true
  | state ->
    [%log.error
      [%here]
        "Ignoring [Heartbeat_ack] in unexpected state"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (state : State.t)]
;;

let rec schedule_heartbeat t ~heartbeat_interval =
  match t.state with
  | Disconnected ->
    [%log.error
      [%here]
        "Cannot schedule heartbeat while disconected"
        ~guild_id:(t.guild_id : Model.Guild_id.t)]
  | Connected { created_at; _ } ->
    let stop_heartbeat = Ivar.create () in
    Time_source.every'
      ~stop:(Ivar.read stop_heartbeat)
      t.time_source
      heartbeat_interval
      (fun () ->
         match t.state with
         | Connected
             ({ created_at = created_at'; last_heartbeat_acked; last_seq_num; _ } as
              connected)
           when [%equal: Time_ns.t] created_at created_at' ->
           if last_heartbeat_acked
           then (
             connected.last_heartbeat_acked <- false;
             send_heartbeat t last_seq_num)
           else (
             [%log.error
               [%here]
                 "Last heartbeat unacked, resuming..."
                 ~guild_id:(t.guild_id : Model.Guild_id.t)];
             Ivar.fill_if_empty stop_heartbeat ();
             resume t)
         | _ ->
           Ivar.fill_if_empty stop_heartbeat ();
           return ());
    [%log.debug [%here] "Heartbeat scheduled" ~guild_id:(t.guild_id : Model.Guild_id.t)]

and handle_hello t { Model.Voice_gateway.Event.Hello.heartbeat_interval } =
  match t.state with
  | Connected { state; heartbeating; _ } ->
    (match Set_once.set heartbeating () with
     | Error _ ->
       [%log.error
         [%here] "Already heartbeating" ~guild_id:(t.guild_id : Model.Guild_id.t)]
     | Ok () -> schedule_heartbeat t ~heartbeat_interval);
    let%bind () =
      match state with
      | Waiting_for_ready -> send_identify t
      | Waiting_for_session_description _ | Ready_to_send _ ->
        [%log.debug
          [%here] "Ready, skipping identify" ~guild_id:(t.guild_id : Model.Guild_id.t)];
        return ()
    in
    return ()
  | state ->
    [%log.error
      [%here]
        "Ignoring [Hello] in unexpected state"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (state : State.t)];
    return ()

and handle_event t (event : Model.Voice_gateway.Event.Receivable.t) =
  match event with
  | Hello hello -> handle_hello t hello
  | Ready ready -> handle_ready t ready
  | Session_description session_description ->
    handle_session_description t session_description;
    return ()
  | Heartbeat_ack { nonce = _ } ->
    handle_heartbeat_ack t;
    return ()
  | Unknown event ->
    [%log.debug
      [%here] "Received unknown event" (event : Websocket_protocol.Voice_gateway.Event.t)];
    return ()

and read_event t payload =
  match
    let%bind.Or_error protocol =
      Or_error.try_with (fun () ->
        Json.from_string payload |> [%of_yojson: Websocket_protocol.Voice_gateway.Event.t])
    in
    (match Websocket_protocol.Voice_gateway.Event.seq_num protocol, t.state with
     | None, _ | _, Disconnected -> ()
     | Some seq_num, Connected connected -> connected.last_seq_num <- Some seq_num);
    Model.Voice_gateway.Event.Receivable.of_protocol_or_error protocol
  with
  | Ok event ->
    [%log.debug
      [%here]
        "Received"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (event : Model.Voice_gateway.Event.Receivable.t)];
    let%bind () = handle_event t event in
    return ()
  | Error error ->
    [%log.error
      [%here]
        "Failed to receive event"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (payload : string)
        (error : Error.t)];
    return ()

and handle_new_connection t state ws =
  t.state <- Connected (State.Connected.create ~time_source:t.time_source state ws);
  don't_wait_for (Pipe.iter (Websocket.reader ws) ~f:(read_event t));
  don't_wait_for
    (let%bind reason, message, info = Websocket.close_finished ws in
     [%log.info
       [%here]
         "Websocket closed"
         ~guild_id:(t.guild_id : Model.Guild_id.t)
         (reason : Websocket.Connection_close_reason.t)
         (message : string)
         (info : Info.t option)];
     match reason with
     | Normal_closure -> disconnect t
     | Unknown ((4014 | 4021 | 4022) as errno) ->
       (* Should not reconnect. *)
       (* https://discord.com/developers/docs/topics/opcodes-and-status-codes#voice-voice-close-event-codes *)
       [%log.error [%here] "Not resuming" (errno : int)];
       return ()
     | Unknown 4006 ->
       (* Session no longer valid *)
       (* https://discord.com/developers/docs/topics/opcodes-and-status-codes#voice-voice-close-event-codes *)
       reincarnate t
     | _ -> resume t)

and resume t =
  match%bind
    match t.state with
    | Disconnected
    | Connected { state = Waiting_for_ready | Waiting_for_session_description _; _ } ->
      Deferred.Or_error.fail
        (Error.create_s
           [%message
             [%here]
               "Unable to resume from current state"
               ~guild_id:(t.guild_id : Model.Guild_id.t)
               (t.state : State.t)])
    | Connected
        { ws = _
        ; state = Ready_to_send ready_to_send
        ; last_seq_num
        ; last_heartbeat_acked = _
        ; heartbeating = _
        ; created_at = _
        } ->
      let%bind () = disconnect t in
      [%log.info [%here] "Resuming..." ~guild_id:(t.guild_id : Model.Guild_id.t)];
      let%bind.Deferred.Or_error response, ws =
        Cohttp_async_websocket.Client.create (formulate_url t.endpoint)
      in
      [%log.info [%here] "Reconnected" ~guild_id:(t.guild_id : Model.Guild_id.t)];
      [%log.debug [%here] (response : Cohttp.Response.t)];
      handle_new_connection t (Ready_to_send ready_to_send) ws;
      let%bind () =
        write_if_connected
          t
          (Resume
             { token = t.token
             ; session_id = t.session_id
             ; server_id = t.guild_id
             ; seq_ack = last_seq_num
             })
      in
      Deferred.Or_error.ok_unit
  with
  | Ok () -> return ()
  | Error error ->
    [%log.error
      [%here]
        "Resuming failed, reincarnating"
        ~guild_id:(t.guild_id : Model.Guild_id.t)
        (error : Error.t)];
    reincarnate t

and connect t =
  match t.state with
  | Connected _ ->
    Deferred.Or_error.fail
      (Error.create_s
         [%message "Already connected" ~guild_id:(t.guild_id : Model.Guild_id.t)])
  | Disconnected ->
    [%log.info [%here] "Connecting..." ~guild_id:(t.guild_id : Model.Guild_id.t)];
    let%bind.Deferred.Or_error response, ws =
      Cohttp_async_websocket.Client.create (formulate_url t.endpoint)
    in
    [%log.info [%here] "Connected" ~guild_id:(t.guild_id : Model.Guild_id.t)];
    [%log.debug [%here] (response : Cohttp.Response.t)];
    handle_new_connection t Waiting_for_ready ws;
    Deferred.Or_error.ok_unit
;;
