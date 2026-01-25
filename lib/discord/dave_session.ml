open! Core
open! Async

(** DAVE protocol transition ID for initialization *)
let init_transition_id = Model.Dave_transition_id.of_int_exn 0

(** Expected epoch for new MLS group *)
let mls_new_group_expected_epoch = Model.Dave_epoch.of_int_exn 1

(** Disabled protocol version *)
let disabled_version = Model.Dave_protocol_version.of_int_exn 0

let max_supported_protocol_version =
  Model.Dave_protocol_version.of_int_exn Dave.max_supported_protocol_version
;;

type t =
  { session : Dave.Session.t
  ; user_id : Model.User_id.t
  ; channel_id : Model.Channel_id.t
  ; recognized_user_ids : Model.User_id.Hash_set.t
  ; protocol_transitions : Model.Dave_protocol_version.t Model.Dave_transition_id.Table.t
  ; mutable latest_prepared_transition_version : Model.Dave_protocol_version.t
  ; events_writer : Model.Voice_gateway.Event.Sendable.t Pipe.Writer.t
  ; encryptor : Dave.Encryptor.t
  ; decryptor : Dave.Decryptor.t
  ; closed : unit Ivar.t
  }
[@@deriving fields ~getters]

let create ~user_id ~channel_id ~users_in_channel =
  let on_error ~source ~reason =
    [%log.error [%here] "MLS Session Error" (source : string) (reason : string)]
  in
  let events_reader, events_writer = Pipe.create () in
  let session = Dave.Session.create ~on_error in
  let encryptor = Dave.Encryptor.create () in
  let decryptor = Dave.Decryptor.create () in
  (* Start in passthrough mode until MLS handshake completes *)
  Dave.Encryptor.set_passthrough_mode encryptor true;
  Dave.Decryptor.set_passthrough_mode decryptor true;
  ( { session
    ; user_id
    ; channel_id
    ; recognized_user_ids = Model.User_id.Hash_set.of_list (user_id :: users_in_channel)
    ; protocol_transitions = Model.Dave_transition_id.Table.create ()
    ; latest_prepared_transition_version = disabled_version
    ; events_writer
    ; encryptor
    ; decryptor
    ; closed = Ivar.create ()
    }
  , events_reader )
;;

let run_if_not_closed' ~(here : [%call_pos]) t f ~default =
  if Ivar.is_full t.closed
  then (
    [%log.debug
      [%here] "DAVE session closed, ignoring action" (here : Source_code_position.t)];
    default)
  else f ()
;;

let run_if_not_closed ~(here : [%call_pos]) t f = run_if_not_closed' ~here t f ~default:()

let close
  ({ session
   ; user_id = _
   ; channel_id = _
   ; recognized_user_ids = _
   ; protocol_transitions = _
   ; latest_prepared_transition_version = _
   ; events_writer
   ; encryptor
   ; decryptor
   ; closed
   } as t)
  =
  let%with () = run_if_not_closed ~here:[%here] t in
  [%log.debug [%here] "Closing DAVE session"];
  Ivar.fill_if_empty closed ();
  Dave.Encryptor.destroy encryptor;
  Dave.Decryptor.destroy decryptor;
  Pipe.close events_writer;
  Dave.Session.destroy session
;;

let send_event t = Pipe.write_without_pushback_if_open t.events_writer

let get_protocol_version t =
  let%with () = run_if_not_closed' t ~default:disabled_version in
  Dave.Session.get_protocol_version t.session |> Model.Dave_protocol_version.of_int_exn
;;

let setup_key_ratchet_for_user t ~user_id ~protocol_version =
  let%with () = run_if_not_closed t in
  let key_ratchet =
    if [%equal: Model.Dave_protocol_version.t] protocol_version disabled_version
    then None
    else Some (Dave.Session.get_key_ratchet t.session (Model.User_id.to_string user_id))
  in
  if [%equal: Model.User_id.t] user_id t.user_id
  then (
    match key_ratchet with
    | None -> Dave.Encryptor.set_passthrough_mode t.encryptor true
    | Some key_ratchet ->
      Dave.Encryptor.set_key_ratchet t.encryptor key_ratchet;
      Dave.Encryptor.set_passthrough_mode t.encryptor false;
      Dave.Key_ratchet.destroy key_ratchet)
  else (
    match key_ratchet with
    | None -> Dave.Decryptor.set_passthrough_mode t.decryptor true
    | Some key_ratchet ->
      Dave.Decryptor.transition_to_key_ratchet t.decryptor key_ratchet;
      Dave.Decryptor.set_passthrough_mode t.decryptor false;
      Dave.Key_ratchet.destroy key_ratchet)
;;

let prepare_dave_protocol_ratchets t ~transition_id ~protocol_version =
  let%with () = run_if_not_closed t in
  [%log.debug
    [%here]
      "Preparing DAVE protocol ratchets"
      (transition_id : Model.Dave_transition_id.t)
      (protocol_version : Model.Dave_protocol_version.t)];
  (* Setup key ratchets for all recognized users except self *)
  Hash_set.iter t.recognized_user_ids ~f:(fun user_id ->
    if not ([%equal: Model.User_id.t] user_id t.user_id)
    then setup_key_ratchet_for_user t ~user_id ~protocol_version);
  (* For init transition, also setup self immediately; otherwise defer to execute *)
  if [%equal: Model.Dave_transition_id.t] transition_id init_transition_id
  then setup_key_ratchet_for_user t ~user_id:t.user_id ~protocol_version
  else Hashtbl.set t.protocol_transitions ~key:transition_id ~data:protocol_version;
  t.latest_prepared_transition_version <- protocol_version
;;

let maybe_send_dave_protocol_ready_for_transition t ~transition_id =
  let%with () = run_if_not_closed t in
  if not ([%equal: Model.Dave_transition_id.t] transition_id init_transition_id)
  then send_event t (Dave_protocol_ready_for_transition { transition_id })
;;

let send_mls_key_package t =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Sending MLS key package"];
  let key_package =
    Dave.Session.get_marshalled_key_package t.session |> Dave.Uint8_data.to_string
  in
  send_event t (Mls_key_package { key_package })
;;

let handle_dave_protocol_prepare_epoch t ~epoch ~protocol_version =
  let%with () = run_if_not_closed t in
  if [%equal: Model.Dave_epoch.t] epoch mls_new_group_expected_epoch
  then
    Dave.Session.init
      t.session
      ~version:(Model.Dave_protocol_version.to_int_exn protocol_version)
      ~group_id:(Model.Channel_id.to_string t.channel_id |> Int.of_string)
      ~self_user_id:(Model.User_id.to_string t.user_id)
;;

let handle_dave_protocol_execute_transition t ~transition_id =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Executing transition" (transition_id : Model.Dave_transition_id.t)];
  match Hashtbl.find_and_remove t.protocol_transitions transition_id with
  | None ->
    [%log.debug
      [%here]
        "Ignoring execute transition for unknown transition"
        (transition_id : Model.Dave_transition_id.t)]
  | Some protocol_version ->
    [%log.debug
      [%here]
        "Found transition, setting up self key ratchet"
        (transition_id : Model.Dave_transition_id.t)
        (protocol_version : Model.Dave_protocol_version.t)];
    if [%equal: Model.Dave_protocol_version.t] protocol_version disabled_version
    then Dave.Session.reset t.session;
    setup_key_ratchet_for_user t ~user_id:t.user_id ~protocol_version
;;

let flag_mls_invalid_commit_welcome t ~transition_id =
  let%with () = run_if_not_closed t in
  send_event t (Mls_invalid_commit_welcome { transition_id })
;;

let handle_dave_protocol_init t ~protocol_version =
  let%with () = run_if_not_closed t in
  if [%equal: Model.Dave_protocol_version.t] protocol_version disabled_version
  then (
    prepare_dave_protocol_ratchets t ~transition_id:init_transition_id ~protocol_version;
    handle_dave_protocol_execute_transition t ~transition_id:init_transition_id)
  else (
    handle_dave_protocol_prepare_epoch
      t
      ~epoch:mls_new_group_expected_epoch
      ~protocol_version;
    send_mls_key_package t)
;;

let create_user t ~user_id =
  let%with () = run_if_not_closed t in
  Hash_set.add t.recognized_user_ids user_id;
  let num_users = Hash_set.length t.recognized_user_ids in
  [%log.debug
    [%here] "Adding recognized user" (user_id : Model.User_id.t) (num_users : int)];
  setup_key_ratchet_for_user
    t
    ~user_id
    ~protocol_version:t.latest_prepared_transition_version
;;

let destroy_user t ~user_id =
  let%with () = run_if_not_closed t in
  Hash_set.remove t.recognized_user_ids user_id;
  let num_users = Hash_set.length t.recognized_user_ids in
  [%log.debug
    [%here] "Removing recognized user" (user_id : Model.User_id.t) (num_users : int)]
;;

let on_clients_connect t { Model.Voice_gateway.Event.Clients_connect.user_ids } =
  let%with () = run_if_not_closed t in
  List.iter user_ids ~f:(fun user_id -> create_user t ~user_id)
;;

let on_client_disconnect t { Model.Voice_gateway.Event.Client_disconnect.user_id } =
  let%with () = run_if_not_closed t in
  destroy_user t ~user_id
;;

let on_session_description t dave_protocol_version =
  let%with () = run_if_not_closed t in
  handle_dave_protocol_init t ~protocol_version:dave_protocol_version
;;

let on_dave_protocol_prepare_transition
  t
  { Model.Voice_gateway.Event.Dave_protocol_prepare_transition.transition_id
  ; protocol_version
  }
  =
  let%with () = run_if_not_closed t in
  prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
  maybe_send_dave_protocol_ready_for_transition t ~transition_id
;;

let on_dave_protocol_execute_transition
  t
  { Model.Voice_gateway.Event.Dave_protocol_execute_transition.transition_id }
  =
  let%with () = run_if_not_closed t in
  handle_dave_protocol_execute_transition t ~transition_id
;;

let on_dave_protocol_prepare_epoch
  t
  { Model.Voice_gateway.Event.Dave_protocol_prepare_epoch.epoch; protocol_version }
  =
  let%with () = run_if_not_closed t in
  handle_dave_protocol_prepare_epoch t ~epoch ~protocol_version;
  if [%equal: Model.Dave_epoch.t] epoch mls_new_group_expected_epoch
  then send_mls_key_package t
;;

let on_mls_external_sender_package
  t
  { Model.Voice_gateway.Event.Mls_external_sender_package.external_sender_package }
  =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Received MLS external sender package"];
  let data = Dave.Uint8_data.of_string external_sender_package in
  Dave.Session.set_external_sender t.session data
;;

let on_mls_proposals t { Model.Voice_gateway.Event.Mls_proposals.proposals } =
  let%with () = run_if_not_closed t in
  let commit_welcome =
    let proposals = Dave.Uint8_data.of_string proposals in
    let recognized_user_ids =
      Hash_set.to_list t.recognized_user_ids |> List.map ~f:Model.User_id.to_string
    in
    Dave.Session.process_proposals t.session ~proposals ~recognized_user_ids
  in
  (* If process_proposals returns non-empty data, send commit_welcome *)
  let commit_welcome = Dave.Uint8_data.to_string commit_welcome in
  if String.is_empty commit_welcome
  then [%log.debug [%here] "process_proposals returned empty, no commit created"]
  else (
    [%log.debug [%here] "Created commit_welcome"];
    send_event t (Mls_commit_welcome { commit_welcome }))
;;

let on_mls_announce_commit_transition
  t
  { Model.Voice_gateway.Event.Mls_announce_commit_transition.transition_id; commit }
  =
  let%with () = run_if_not_closed t in
  [%log.debug
    [%here]
      "Received MLS prepare commit transition"
      (transition_id : Model.Dave_transition_id.t)];
  let commit_data = Dave.Uint8_data.of_string commit in
  let commit_result = Dave.Session.process_commit t.session commit_data in
  let protocol_version = get_protocol_version t in
  let is_ignored = Dave.Commit_result.is_ignored commit_result in
  let is_failed = Dave.Commit_result.is_failed commit_result in
  [%log.debug [%here] "process_commit returned" (is_ignored : bool) (is_failed : bool)];
  if is_ignored
  then
    [%log.debug
      [%here] "MLS commit was ignored" (transition_id : Model.Dave_transition_id.t)]
  else if is_failed
  then (
    [%log.error [%here] "MLS commit failed" (transition_id : Model.Dave_transition_id.t)];
    flag_mls_invalid_commit_welcome t ~transition_id;
    handle_dave_protocol_init t ~protocol_version)
  else (
    prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
    maybe_send_dave_protocol_ready_for_transition t ~transition_id);
  Dave.Commit_result.destroy commit_result
;;

let on_mls_welcome t { Model.Voice_gateway.Event.Mls_welcome.transition_id; welcome } =
  let%with () = run_if_not_closed t in
  let recognized_user_ids =
    Hash_set.to_list t.recognized_user_ids |> List.map ~f:Model.User_id.to_string
  in
  let num_users = List.length recognized_user_ids in
  [%log.debug
    [%here]
      "Received MLS welcome"
      (transition_id : Model.Dave_transition_id.t)
      (num_users : int)];
  let welcome_result =
    let welcome_data = Dave.Uint8_data.of_string welcome in
    Dave.Session.process_welcome t.session welcome_data ~recognized_user_ids
  in
  (* Check if we joined the group by looking at roster member IDs *)
  let rosters = Dave.Welcome_result.get_roster_member_ids welcome_result in
  let joined_group = not (Dave.Uint64_data.is_empty rosters) in
  [%log.debug
    [%here]
      "process_welcome result"
      (transition_id : Model.Dave_transition_id.t)
      (joined_group : bool)];
  if joined_group
  then (
    let protocol_version = get_protocol_version t in
    [%log.debug
      [%here]
        "Successfully joined group via welcome"
        (protocol_version : Model.Dave_protocol_version.t)];
    prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
    maybe_send_dave_protocol_ready_for_transition t ~transition_id)
  else (
    (* Welcome didn't result in joining group - flag invalid and send new key package *)
    [%log.debug
      [%here]
        "MLS welcome didn't result in joining group"
        (transition_id : Model.Dave_transition_id.t)];
    flag_mls_invalid_commit_welcome t ~transition_id;
    send_mls_key_package t);
  Dave.Welcome_result.destroy welcome_result
;;

let encrypt t ~ssrc ~plaintext =
  let%with () = run_if_not_closed' t ~default:plaintext in
  let result, ciphertext =
    Dave.Encryptor.encrypt
      t.encryptor
      ~media_type:Audio
      ~ssrc:(Model.Ssrc.to_int_exn ssrc)
      ~plaintext
  in
  match result with
  | Success -> ciphertext
  | error ->
    (match error with
     | Success | Missing_key_ratchet -> ()
     | Encryption_failure | Missing_cryptor | Too_many_attempts ->
       [%log.error
         [%here] "DAVE encryption failed" (error : Dave.Encryptor.Result_code.t)]);
    plaintext
;;

let decrypt t ~ciphertext =
  let%with () = run_if_not_closed' t ~default:ciphertext in
  let result, plaintext =
    Dave.Decryptor.decrypt t.decryptor ~media_type:Audio ~ciphertext
  in
  match result with
  | Success -> plaintext
  | error ->
    (match error with
     | Success | Missing_key_ratchet -> ()
     | Decryption_failure | Missing_cryptor | Invalid_nonce ->
       [%log.error
         [%here] "DAVE decryption failed" (error : Dave.Decryptor.Result_code.t)]);
    ciphertext
;;

let assign_ssrc_to_codec t ~ssrc ~codec =
  let%with () = run_if_not_closed t in
  Dave.Encryptor.assign_ssrc_to_codec
    t.encryptor
    ~ssrc:(Model.Ssrc.to_int_exn ssrc)
    ~codec
;;
