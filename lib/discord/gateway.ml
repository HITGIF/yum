open! Core
open! Async
module Json = Common.Json

module Websocket = struct
  include Websocket

  let reader t = pipes t |> fst
  let writer t = pipes t |> snd
end

module Event = struct
  type t =
    | Message of Model.Message.t
    | Voice_connected of { guild_id : Model.Guild_id.t }
    | Voice of
        { guild_id : Model.Guild_id.t
        ; event : Voice_gateway.Event.t
        }
end

module Attempt : sig
  type t [@@deriving sexp_of]

  val create : ?reset_after:Time_ns.Span.t -> max:int -> unit -> t
  val try_ : t -> unit Or_error.t
  val reset : t -> unit
end = struct
  type t =
    { max : int
    ; reset_after : Time_ns.Span.t option
    ; mutable remaining : int
    ; mutable last_attempt : Time_ns.t
    }
  [@@deriving sexp_of]

  let create ?reset_after ~max () =
    assert (max >= 0);
    { max; reset_after; remaining = max; last_attempt = Time_ns.epoch }
  ;;

  let reset t = t.remaining <- t.max

  let try_ t =
    let now = Time_ns.now () in
    (match t.reset_after with
     | None -> ()
     | Some reset_after ->
       let reset_after = Time_ns.add_saturating t.last_attempt reset_after in
       if Time_ns.O.(now > reset_after) then reset t);
    t.last_attempt <- now;
    if t.remaining > 0
    then (
      t.remaining <- t.remaining - 1;
      Ok ())
    else Or_error.error_s [%message "Exceeded max attempts" ~max_attempts:(t.max : int)]
  ;;
end

module State = struct
  module Disconnected = struct
    type t = { connected : unit Ivar.t } [@@deriving sexp_of]

    let create () = { connected = Ivar.create () }
  end

  module Connected = struct
    module State = struct
      module Voice_state = struct
        type t =
          { channel_id : Model.Channel_id.t option
          ; session_id : Model.Voice_gateway_session_id.t
          }
        [@@deriving fields ~getters, sexp_of]
      end

      module Voice_server = struct
        type t =
          { token : Model.Voice_connection_token.t
          ; endpoint : Model.Uri.t option
          }
        [@@deriving sexp_of]
      end

      type t =
        | Not_ready of { ready : unit Ivar.t }
        | Ready of
            { session_id : Model.Gateway_session_id.t
            ; resume_gateway_url : Model.Uri.t
            ; user : Model.User.t
            ; voice_states : Voice_state.t Model.User_id.Table.t Model.Guild_id.Table.t
            ; voice_servers : Voice_server.t Model.Guild_id.Table.t
            ; voice_gateways : (Voice_gateway.t[@sexp.opaque]) Model.Guild_id.Table.t
            ; voice_reincarnation_attempts : Attempt.t Model.Guild_id.Table.t
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
    | Disconnected of Disconnected.t
    | Connected of Connected.t
  [@@deriving sexp_of]
end

type t =
  { mutable state : State.t
  ; time_source : Time_source.t
  ; initial_gateway_url : Model.Uri.t
  ; auth_token : Model.Auth_token.t
  ; intents : Model.Intents.t
  ; properties : Model.Gateway.Event.Identify.Properties.t
  ; events_reader : Event.t Pipe.Reader.t
  ; events_writer : Event.t Pipe.Writer.t
  }

let create ?time_source ~initial_gateway_url ~auth_token ~intents ~properties () =
  let time_source =
    match time_source with
    | None -> Time_source.wall_clock ()
    | Some time_source -> Time_source.read_only time_source
  in
  let events_reader, events_writer = Pipe.create () in
  { state = Disconnected (State.Disconnected.create ())
  ; time_source
  ; initial_gateway_url
  ; auth_token
  ; intents = Model.Intents.create intents
  ; properties
  ; events_reader
  ; events_writer
  }
;;

let rec wait_until_ready t =
  match t.state with
  | Connected { state = Ready _; _ } -> return ()
  | Connected { state = Not_ready { ready }; _ } ->
    let%bind () = Ivar.read ready in
    wait_until_ready t
  | Disconnected { connected } ->
    let%bind () = Ivar.read connected in
    wait_until_ready t
;;

let events t = t.events_reader
let emit t event = Pipe.write_without_pushback_if_open t.events_writer event

let rec write_upon_connected' t get_event =
  match t.state with
  | Disconnected { connected } ->
    [%log.debug [%here] "Attempting write when disconnected, waiting for connection"];
    let%bind () = Ivar.read connected in
    write_upon_connected' t get_event
  | Connected connected ->
    let event = get_event connected in
    (match%map
       Deferred.Or_error.try_with_join (fun () ->
         event
         |> Model.Gateway.Event.Sendable.to_protocol
         |> [%yojson_of: Websocket_protocol.Gateway.Event.t]
         |> Json.to_string
         |> Pipe.write_if_open (Websocket.writer connected.ws)
         |> Deferred.ok)
     with
     | Ok () -> [%log.debug [%here] "Sent" (event : Model.Gateway.Event.Sendable.t)]
     | Error error ->
       [%log.error
         "Failed to write gateway event"
           (event : Model.Gateway.Event.Sendable.t)
           (error : Error.t)])
;;

let write_upon_connected t event = write_upon_connected' t (Fn.const event)

let send_heartbeat t =
  write_upon_connected' t (fun { State.Connected.last_seq_num; _ } ->
    Heartbeat { last_seq_num })
;;

let send_identify t =
  write_upon_connected
    t
    (Identify { token = t.auth_token; intents = t.intents; properties = t.properties })
;;

let send_voice_server_update t ~guild_id ~channel_id =
  write_upon_connected
    t
    (Voice_state_update { guild_id; channel_id; self_mute = false; self_deaf = false })
;;

let join_voice t ~guild_id ~channel_id =
  send_voice_server_update t ~guild_id ~channel_id:(Some channel_id)
;;

let rec join_user_voice t ~guild_id ~user_id =
  match t.state with
  | Connected { state = Ready { voice_states; _ }; _ } ->
    (match
       let%bind.Option guild_voice_states = Hashtbl.find voice_states guild_id in
       let%bind.Option { channel_id; _ } = Hashtbl.find guild_voice_states user_id in
       channel_id
     with
     | None -> return `User_not_in_voice_channel
     | Some channel_id ->
       let%bind () = join_voice t ~guild_id ~channel_id in
       return (`Ok channel_id))
  | _ ->
    let%bind () = wait_until_ready t in
    join_user_voice t ~guild_id ~user_id
;;

let leave_voice t ~guild_id = send_voice_server_update t ~guild_id ~channel_id:None

let handle_ready
  t
  { Model.Gateway.Event.Dispatch.Ready.session_id
  ; resume_gateway_url
  ; user
  ; v = _
  ; guilds = _
  ; application = _
  }
  =
  match t.state with
  | Connected ({ state = Not_ready { ready }; _ } as connected) ->
    connected.state
    <- Ready
         { session_id
         ; resume_gateway_url
         ; user
         ; voice_states = Model.Guild_id.Table.create ()
         ; voice_servers = Model.Guild_id.Table.create ()
         ; voice_gateways = Model.Guild_id.Table.create ()
         ; voice_reincarnation_attempts = Model.Guild_id.Table.create ()
         };
    Ivar.fill_if_empty ready ();
    [%log.info [%here] "Ready" (session_id : Model.Gateway_session_id.t)]
  | state -> [%log.error [%here] "Ignoring [Ready] in unexpected state" (state : State.t)]
;;

let handle_guild_create
  t
  { Model.Gateway.Event.Dispatch.Guild_create.id = guild_id
  ; voice_states = new_voice_states
  ; name = _
  ; unavailable = _
  }
  =
  match t.state with
  | Connected { state = Ready { voice_states; _ }; _ } ->
    Hashtbl.set
      voice_states
      ~key:guild_id
      ~data:
        (List.map
           new_voice_states
           ~f:(fun { user_id; channel_id; session_id; guild_id = _ } ->
             user_id, { State.Connected.State.Voice_state.channel_id; session_id })
         |> Model.User_id.Table.of_alist_exn)
  | state ->
    [%log.error [%here] "Ignoring [Guild_create] in unexpected state" (state : State.t)]
;;

let formulate_url base = Model.Gateway.Api_version.(ws_url v10 ~base) |> Model.Uri.to_uri

let leave_all_voice t =
  match t.state with
  | Connected { state = Ready { voice_gateways; _ }; _ } ->
    let%bind () =
      Hashtbl.keys voice_gateways
      |> Deferred.List.iter ~how:`Sequential ~f:(fun guild_id -> leave_voice t ~guild_id)
    in
    (* CR-someday: Find something better to wait on... *)
    Clock_ns.after (Time_ns.Span.of_int_ms 1000)
  | _ -> return ()
;;

let rejoin_all_voices t user voice_states =
  let voice_states =
    Model.Guild_id.Map.of_hashtbl_exn voice_states
    |> Map.filter_map ~f:(fun guild_voice_states ->
      Hashtbl.find guild_voice_states user.Model.User.id
      |> Option.bind ~f:State.Connected.State.Voice_state.channel_id)
  in
  Deferred.Map.iteri
    ~how:`Sequential
    voice_states
    ~f:(fun ~key:guild_id ~data:channel_id -> join_voice t ~guild_id ~channel_id)
;;

let disconnect t =
  match t.state with
  | Disconnected disconnected ->
    [%log.debug [%here] "Already disconnected"];
    return disconnected
  | Connected
      { ws
      ; state = _
      ; last_seq_num = _
      ; last_heartbeat_acked = _
      ; heartbeating = _
      ; created_at = _
      } ->
    [%log.info [%here] "Disconnecting..."];
    let reader = Websocket.reader ws in
    Pipe.close_read reader;
    let%bind _ = Websocket.close_finished ws in
    let disconnected = State.Disconnected.create () in
    t.state <- Disconnected disconnected;
    [%log.info [%here] "Disconnected"];
    return disconnected
;;

let handle_heartbeat_ack t =
  match t.state with
  | Connected connected -> connected.last_heartbeat_acked <- true
  | state ->
    [%log.error [%here] "Ignoring [Heartbeat_ack] in unexpected state" (state : State.t)]
;;

let rec schedule_heartbeat t ~heartbeat_interval =
  match t.state with
  | Disconnected _ -> [%log.error [%here] "Cannot schedule heartbeat while disconected"]
  | Connected { created_at; _ } ->
    let stop_heartbeat = Ivar.create () in
    Time_source.every'
      ~stop:(Ivar.read stop_heartbeat)
      t.time_source
      heartbeat_interval
      (fun () ->
         match t.state with
         | Connected ({ created_at = created_at'; last_heartbeat_acked; _ } as connected)
           when [%equal: Time_ns.t] created_at created_at' ->
           if last_heartbeat_acked
           then (
             connected.last_heartbeat_acked <- false;
             send_heartbeat t)
           else (
             [%log.error [%here] "Last heartbeat unacked, resuming..."];
             Ivar.fill_if_empty stop_heartbeat ();
             resume t)
         | _ ->
           Ivar.fill_if_empty stop_heartbeat ();
           return ());
    [%log.debug [%here] "Heartbeat scheduled"]

and handle_hello t { Model.Gateway.Event.Hello.heartbeat_interval } =
  match t.state with
  | Connected { state; heartbeating; _ } ->
    (match Set_once.set heartbeating () with
     | Error _ -> [%log.error [%here] "Already heartbeating"]
     | Ok () -> schedule_heartbeat t ~heartbeat_interval);
    let%bind () =
      match state with
      | Not_ready _ -> send_identify t
      | Ready _ ->
        [%log.debug [%here] "Ready, skipping identify"];
        return ()
    in
    return ()
  | state ->
    [%log.error [%here] "Ignoring [Hello] in unexpected state" (state : State.t)];
    return ()

and update_voice_gateway t ~guild_id ~force_reconnect =
  [%log.debug
    [%here]
      "Updating voice gateway"
      (guild_id : Model.Guild_id.t)
      (force_reconnect
       : [ `Voice_server_update | `Voice_state_update | `Requested_by_client ] option)];
  match t.state with
  | Connected
      { state =
          Ready
            { voice_states
            ; voice_servers
            ; voice_gateways
            ; voice_reincarnation_attempts
            ; user = { id = user_id; _ }
            ; _
            }
      ; _
      } ->
    let desired_state =
      let voice_state =
        let%bind.Option guild_voice_states = Hashtbl.find voice_states guild_id in
        Hashtbl.find guild_voice_states user_id
      in
      let voice_server = Hashtbl.find voice_servers guild_id in
      match voice_state, voice_server with
      | (None | Some { channel_id = None; _ }), _ | _, (None | Some { endpoint = None; _ })
        -> `Should_be_disconnected
      | ( Some { channel_id = Some channel_id; session_id }
        , Some { token; endpoint = Some endpoint } ) ->
        `Should_be_connected (~channel_id, ~session_id, ~token, ~endpoint)
    in
    let disconnect voice_gateway =
      [%log.info [%here] "Closing voice gateway" (guild_id : Model.Guild_id.t)];
      let%bind () = Voice_gateway.close voice_gateway in
      Hashtbl.remove voice_gateways guild_id;
      return ()
    in
    let connect ~channel_id ~session_id ~token ~endpoint =
      [%log.info [%here] "Connecting voice gateway" (guild_id : Model.Guild_id.t)];
      let reincarnate () =
        let max_attempts = 1 in
        let reset_after = Time_ns.Span.minute in
        let attempt =
          Hashtbl.find_or_add
            voice_reincarnation_attempts
            guild_id
            ~default:(Attempt.create ~reset_after ~max:max_attempts)
        in
        match Attempt.try_ attempt with
        | Error error ->
          [%log.error
            [%here]
              "Aborting reincarnation"
              (error : Error.t)
              (guild_id : Model.Guild_id.t)
              (channel_id : Model.Channel_id.t)];
          Attempt.reset attempt;
          return ()
        | Ok () ->
          [%log.info
            [%here]
              "Reincarnating voice gateway"
              (attempt : Attempt.t)
              (guild_id : Model.Guild_id.t)
              (channel_id : Model.Channel_id.t)];
          join_voice t ~guild_id ~channel_id
      in
      let voice_gateway =
        Voice_gateway.create
          ~time_source:t.time_source
          ~guild_id
          ~channel_id
          ~token
          ~endpoint
          ~session_id
          ~user_id
          ~reincarnate
          ()
      in
      match%map Voice_gateway.connect voice_gateway with
      | Ok () ->
        Hashtbl.set voice_gateways ~key:guild_id ~data:voice_gateway;
        don't_wait_for
          (Voice_gateway.events voice_gateway
           |> Pipe.iter ~f:(fun event ->
             emit t (Voice { guild_id; event });
             return ()));
        emit t (Voice_connected { guild_id })
      | Error error ->
        [%log.error
          [%here]
            "Error connecting to voice gateway, aborting attempt to join"
            (guild_id : Model.Guild_id.t)
            (channel_id : Model.Channel_id.t)
            (error : Error.t)];
        Hashtbl.find voice_states guild_id
        |> Option.iter ~f:(Fn.flip Hashtbl.remove user_id)
    in
    (match desired_state, Hashtbl.find voice_gateways guild_id with
     | `Should_be_disconnected, None ->
       [%log.debug [%here] "Disconnected as expected."];
       return ()
     | `Should_be_disconnected, Some voice_gateway ->
       [%log.debug [%here] "Should not be connected but is, disconnecting..."];
       disconnect voice_gateway
     | `Should_be_connected (~channel_id, ~session_id, ~token, ~endpoint), None ->
       [%log.debug [%here] "Should be connected but not, connecting..."];
       connect ~channel_id ~session_id ~token ~endpoint
     | ( `Should_be_connected (~channel_id, ~session_id, ~token, ~endpoint)
       , Some voice_gateway ) ->
       let reconnect () =
         let%bind () = disconnect voice_gateway in
         let%bind () = connect ~channel_id ~session_id ~token ~endpoint in
         return ()
       in
       (match force_reconnect with
        | Some reason ->
          [%log.info
            [%here]
              "Voice gateway reconnection forced, reconnecting..."
              (reason
               : [ `Voice_server_update | `Voice_state_update | `Requested_by_client ])];
          reconnect ()
        | None ->
          let expected_connection_params = ~session_id, ~token, ~endpoint in
          let actual_connection_params =
            ( ~session_id:(Voice_gateway.session_id voice_gateway)
            , ~token:(Voice_gateway.token voice_gateway)
            , ~endpoint:(Voice_gateway.endpoint voice_gateway) )
          in
          let connected_as_expected =
            [%equal:
              session_id:Model.Voice_gateway_session_id.t
              * token:Model.Voice_connection_token.t
              * endpoint:Model.Uri.t]
              expected_connection_params
              actual_connection_params
          in
          if connected_as_expected
          then (
            [%log.debug [%here] "Connected as expected."];
            return ())
          else (
            [%log.error
              [%here]
                "Current voice gateway connection params are unexpected, reconnecting..."
                (expected_connection_params
                 : session_id:Model.Voice_gateway_session_id.t
                   * token:Model.Voice_connection_token.t
                   * endpoint:Model.Uri.t)
                (actual_connection_params
                 : session_id:Model.Voice_gateway_session_id.t
                   * token:Model.Voice_connection_token.t
                   * endpoint:Model.Uri.t)];
            reconnect ())))
  | _ -> return ()

and handle_voice_state_update
  t
  ({ Model.Gateway.Event.Dispatch.Voice_state.guild_id; user_id; channel_id; session_id }
   as event)
  =
  match t.state with
  | Connected { state = Ready { voice_states; user = { id = my_user_id; _ }; _ }; _ } ->
    (match guild_id with
     | Some guild_id ->
       Hashtbl.find_or_add voice_states guild_id ~default:Model.User_id.Table.create
       |> Hashtbl.set ~key:user_id ~data:{ channel_id; session_id };
       if [%equal: Model.User_id.t] user_id my_user_id
       then update_voice_gateway t ~guild_id ~force_reconnect:(Some `Voice_state_update)
       else return ()
     | None ->
       [%log.error
         [%here]
           "Ignoring [Voice_state_update] without [guild_id]"
           (event : Model.Gateway.Event.Dispatch.Voice_state.t)];
       return ())
  | state ->
    [%log.error
      [%here] "Ignoring [Voice_state_update] in unexpected state" (state : State.t)];
    return ()

and handle_voice_server_update
  t
  { Model.Gateway.Event.Dispatch.Voice_server_update.guild_id; token; endpoint }
  =
  match t.state with
  | Connected { state = Ready { voice_servers; _ }; _ } ->
    Hashtbl.set voice_servers ~key:guild_id ~data:{ token; endpoint };
    update_voice_gateway t ~guild_id ~force_reconnect:(Some `Voice_server_update)
  | state ->
    [%log.error
      [%here] "Ignoring [Voice_server_update] in unexpected state" (state : State.t)];
    return ()

and handle_event t (event : Model.Gateway.Event.Receivable.t) =
  match event with
  | Hello hello -> handle_hello t hello
  | Heartbeat -> send_heartbeat t
  | Reconnect
  | Invalid_session
      { resumable = (* Doen't matter, if resuming failes we'll restart anyways *)
                    _ } -> resume t
  | Dispatch (Ready ready) ->
    handle_ready t ready;
    return ()
  | Dispatch Resumed ->
    [%log.info "Resumed"];
    return ()
  | Dispatch (Guild_create guild_create) ->
    handle_guild_create t guild_create;
    return ()
  | Dispatch (Voice_state_update voice_state) -> handle_voice_state_update t voice_state
  | Dispatch (Voice_server_update voice_server_update) ->
    handle_voice_server_update t voice_server_update
  | Dispatch (Message_create message) ->
    emit t (Message message);
    return ()
  | Heartbeat_ack ->
    handle_heartbeat_ack t;
    return ()
  | Dispatch (Unknown { name; data }) ->
    [%log.debug
      [%here] "Received unknown [Dispatch] event" (name : string) (data : Json.t)];
    return ()
  | Unknown event ->
    [%log.debug
      [%here] "Received unknown event" (event : Websocket_protocol.Gateway.Event.t)];
    return ()

and read_event t payload =
  match
    Or_error.try_with_join (fun () ->
      let protocol =
        Json.from_string payload |> [%of_yojson: Websocket_protocol.Gateway.Event.t]
      in
      (match Websocket_protocol.Gateway.Event.seq_num protocol, t.state with
       | None, _ | _, Disconnected _ -> ()
       | Some seq_num, Connected connected -> connected.last_seq_num <- Some seq_num);
      Model.Gateway.Event.Receivable.of_protocol_or_error protocol)
  with
  | Ok event ->
    [%log.debug [%here] "Received" (event : Model.Gateway.Event.Receivable.t)];
    let%bind () = handle_event t event in
    return ()
  | Error error ->
    [%log.error [%here] "Failed to receive event" (payload : string) (error : Error.t)];
    return ()

and handle_new_connection ~disconnected:{ State.Disconnected.connected } t state ws =
  t.state <- Connected (State.Connected.create ~time_source:t.time_source state ws);
  Ivar.fill_if_empty connected ();
  don't_wait_for (Pipe.iter (Websocket.reader ws) ~f:(read_event t));
  don't_wait_for
    (let%bind reason, message, info = Websocket.close_finished ws in
     [%log.info
       [%here]
         "Websocket closed"
         (reason : Websocket.Connection_close_reason.t)
         (message : string)
         (info : Info.t option)];
     match reason with
     | Normal_closure ->
       let%bind (_ : State.Disconnected.t) = disconnect t in
       return ()
     | Unknown ((4004 | 4010 | 4011 | 4012 | 4013 | 4014) as errno) ->
       (* Should not reconnect. *)
       (* https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-close-event-codes *)
       [%log.error [%here] "Not resuming" (errno : int)];
       return ()
     | _ -> resume t)

and resume t =
  match t.state with
  | Disconnected _ | Connected { state = Not_ready _; _ } ->
    [%log.error
      [%here] "Unable to resume from current state, restarting.." (t.state : State.t)];
    restart t
  | Connected
      { ws = _
      ; state = Ready ({ session_id; resume_gateway_url; voice_states; user; _ } as ready)
      ; last_seq_num
      ; last_heartbeat_acked = _
      ; heartbeating = _
      ; created_at = _
      } ->
    let%bind disconnected = disconnect t in
    [%log.info [%here] "Resuming..."];
    (match%bind
       Cohttp_async_websocket.Client.create (formulate_url resume_gateway_url)
     with
     | Ok (response, ws) ->
       [%log.info [%here] "Reconnected"];
       [%log.debug [%here] (response : Cohttp.Response.t)];
       handle_new_connection ~disconnected t (Ready ready) ws;
       let%bind () =
         write_upon_connected
           t
           (Resume { token = t.auth_token; session_id; seq = last_seq_num })
       in
       rejoin_all_voices t user voice_states
     | Error error ->
       [%log.error [%here] "Resuming failed, restarting" (error : Error.t)];
       let%bind () = restart t in
       let%bind () = wait_until_ready t in
       rejoin_all_voices t user voice_states)

and connect t =
  match t.state with
  | Connected _ -> Deferred.Or_error.fail (Error.create_s [%message "Already connected"])
  | Disconnected disconnected ->
    [%log.info [%here] "Connecting..."];
    let%bind.Deferred.Or_error response, ws =
      Cohttp_async_websocket.Client.create (formulate_url t.initial_gateway_url)
    in
    [%log.info [%here] "Connected"];
    [%log.debug [%here] (response : Cohttp.Response.t)];
    handle_new_connection ~disconnected t (Not_ready { ready = Ivar.create () }) ws;
    Deferred.Or_error.ok_unit

and restart t =
  [%log.info [%here] "Restarting..."];
  let%bind (_ : State.Disconnected.t) = disconnect t in
  match%bind connect t with
  | Ok () -> return ()
  | Error error -> Error.raise_s [%message [%here] "Unable to restart" (error : Error.t)]
;;

let reconnect_voice t ~guild_id =
  update_voice_gateway t ~guild_id ~force_reconnect:(Some `Requested_by_client)
;;

let with_ ?time_source ~initial_gateway_url ~auth_token ~intents ~properties f =
  let t = create ?time_source ~initial_gateway_url ~auth_token ~intents ~properties () in
  let%bind.Deferred.Or_error () = connect t in
  let%bind () = wait_until_ready t in
  let%bind ret = f t in
  let%bind () = leave_all_voice t in
  let%bind (_ : State.Disconnected.t) = disconnect t in
  return ret
;;
