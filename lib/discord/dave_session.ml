open! Core
open! Async

(** DAVE protocol transition ID for initialization *)
let init_transition_id = 0

(** Expected epoch for new MLS group *)
let mls_new_group_expected_epoch = 1

(** Disabled protocol version *)
let disabled_version = 0

module Outgoing = struct
  type t =
    | Mls_key_package of { key_package : string }
    | Dave_protocol_ready_for_transition of { transition_id : int }
    | Mls_commit_welcome of { commit_welcome : string }
    | Mls_invalid_commit_welcome of { transition_id : int }
  [@@deriving sexp_of]
end

type t =
  { session : Dave.Session.t
  ; self_user_id : string
  ; group_id : int
  ; recognized_user_ids : String.Hash_set.t
  ; protocol_transitions : int Int.Table.t
  ; outgoing_writer : Outgoing.t Pipe.Writer.t
  ; outgoing_reader : Outgoing.t Pipe.Reader.t
  ; encryptor : Dave.Encryptor.t
  ; decryptor : Dave.Decryptor.t
  ; closed : unit Ivar.t
  }
[@@deriving fields ~getters]

let create ~self_user_id ~group_id =
  let on_error ~source ~reason =
    [%log.error [%here] "MLS failure" (source : string) (reason : string)]
  in
  let outgoing_reader, outgoing_writer = Pipe.create () in
  let session = Dave.Session.create ~on_error in
  let encryptor = Dave.Encryptor.create () in
  let decryptor = Dave.Decryptor.create () in
  (* Start in passthrough mode until MLS handshake completes *)
  Dave.Encryptor.set_passthrough_mode encryptor true;
  Dave.Decryptor.set_passthrough_mode decryptor true;
  { session
  ; self_user_id
  ; group_id
  ; recognized_user_ids = String.Hash_set.create ()
  ; protocol_transitions = Int.Table.create ()
  ; outgoing_writer
  ; outgoing_reader
  ; encryptor
  ; decryptor
  ; closed = Ivar.create ()
  }
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
   ; self_user_id = _
   ; group_id = _
   ; recognized_user_ids = _
   ; protocol_transitions = _
   ; outgoing_writer
   ; outgoing_reader = _
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
  Dave.Session.destroy session;
  Pipe.close outgoing_writer
;;

let outgoing t = t.outgoing_reader
let send_outgoing t msg = Pipe.write_without_pushback_if_open t.outgoing_writer msg

let setup_key_ratchet_for_user t ~user_id ~protocol_version =
  let%with () = run_if_not_closed t in
  let key_ratchet =
    if protocol_version = disabled_version
    then None
    else Some (Dave.Session.get_key_ratchet t.session user_id)
  in
  if String.equal user_id t.self_user_id
  then (
    match key_ratchet with
    | None -> Dave.Encryptor.set_passthrough_mode t.encryptor true
    | Some key_ratchet ->
      Dave.Encryptor.set_key_ratchet t.encryptor key_ratchet;
      Dave.Encryptor.set_passthrough_mode t.encryptor false)
  else (
    match key_ratchet with
    | None -> Dave.Decryptor.set_passthrough_mode t.decryptor true
    | Some key_ratchet ->
      Dave.Decryptor.transition_to_key_ratchet t.decryptor key_ratchet;
      Dave.Decryptor.set_passthrough_mode t.decryptor false)
;;

let prepare_dave_protocol_ratchets t ~transition_id ~protocol_version =
  let%with () = run_if_not_closed t in
  [%log.debug
    [%here]
      "Preparing DAVE protocol ratchets"
      (transition_id : int)
      (protocol_version : int)];
  (* Setup key ratchets for all recognized users except self *)
  Hash_set.iter t.recognized_user_ids ~f:(fun user_id ->
    if not (String.equal user_id t.self_user_id)
    then setup_key_ratchet_for_user t ~user_id ~protocol_version);
  (* For init transition, also setup self immediately; otherwise defer to execute *)
  if transition_id = init_transition_id
  then setup_key_ratchet_for_user t ~user_id:t.self_user_id ~protocol_version
  else Hashtbl.set t.protocol_transitions ~key:transition_id ~data:protocol_version
;;

let maybe_send_dave_protocol_ready_for_transition t ~transition_id =
  let%with () = run_if_not_closed t in
  if transition_id <> init_transition_id
  then send_outgoing t (Dave_protocol_ready_for_transition { transition_id })
;;

let send_mls_key_package t =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Sending MLS key package"];
  let key_package =
    Dave.Session.get_marshalled_key_package t.session
    |> Dave.Uint8_data.to_string
    |> Base64.encode_string
  in
  send_outgoing t (Mls_key_package { key_package })
;;

let handle_dave_protocol_prepare_epoch t ~epoch ~protocol_version ~group_id =
  let%with () = run_if_not_closed t in
  if epoch = mls_new_group_expected_epoch
  then
    Dave.Session.init
      t.session
      ~version:protocol_version
      ~group_id
      ~self_user_id:t.self_user_id
;;

let handle_dave_protocol_execute_transition t ~transition_id =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Executing transition" (transition_id : int)];
  match Hashtbl.find_and_remove t.protocol_transitions transition_id with
  | None ->
    [%log.debug
      [%here] "Ignoring execute transition for unknown transition" (transition_id : int)]
  | Some protocol_version ->
    [%log.debug
      [%here]
        "Found transition, setting up self key ratchet"
        (transition_id : int)
        (protocol_version : int)];
    if protocol_version = disabled_version then Dave.Session.reset t.session;
    setup_key_ratchet_for_user t ~user_id:t.self_user_id ~protocol_version
;;

let flag_mls_invalid_commit_welcome t ~transition_id =
  let%with () = run_if_not_closed t in
  send_outgoing t (Mls_invalid_commit_welcome { transition_id })
;;

let handle_dave_protocol_init t ~protocol_version =
  let%with () = run_if_not_closed t in
  if protocol_version = disabled_version
  then (
    prepare_dave_protocol_ratchets t ~transition_id:init_transition_id ~protocol_version;
    handle_dave_protocol_execute_transition t ~transition_id:init_transition_id)
  else (
    handle_dave_protocol_prepare_epoch
      t
      ~epoch:mls_new_group_expected_epoch
      ~protocol_version
      ~group_id:t.group_id;
    send_mls_key_package t)
;;

let create_user t ~user_id =
  let%with () = run_if_not_closed t in
  Hash_set.add t.recognized_user_ids user_id;
  let num_users = Hash_set.length t.recognized_user_ids in
  [%log.debug [%here] "Adding recognized user" (user_id : string) (num_users : int)];
  let protocol_version = Dave.Session.get_protocol_version t.session in
  setup_key_ratchet_for_user t ~user_id ~protocol_version
;;

let destroy_user t ~user_id =
  let%with () = run_if_not_closed t in
  Hash_set.remove t.recognized_user_ids user_id;
  let num_users = Hash_set.length t.recognized_user_ids in
  [%log.debug [%here] "Removing recognized user" (user_id : string) (num_users : int)]
;;

let on_clients_connect t { Model.Voice_gateway.Event.Clients_connect.user_ids } =
  let%with () = run_if_not_closed t in
  List.iter user_ids ~f:(fun user_id -> create_user t ~user_id)
;;

let on_client_disconnect t { Model.Voice_gateway.Event.Client_disconnect.user_id } =
  let%with () = run_if_not_closed t in
  destroy_user t ~user_id
;;

let on_session_description t ~dave_protocol_version =
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
  handle_dave_protocol_prepare_epoch t ~epoch ~protocol_version ~group_id:t.group_id;
  if epoch = mls_new_group_expected_epoch then send_mls_key_package t
;;

let on_mls_external_sender_package
  t
  { Model.Voice_gateway.Event.Mls_external_sender_package.external_sender_package }
  =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Received MLS external sender package"];
  let data = Base64.decode_exn external_sender_package |> Dave.Uint8_data.of_string in
  Dave.Session.set_external_sender t.session data
;;

let on_mls_proposals t { Model.Voice_gateway.Event.Mls_proposals.proposals } =
  let%with () = run_if_not_closed t in
  let commit_welcome =
    let recognized_user_ids = Hash_set.to_list t.recognized_user_ids in
    let proposals = Base64.decode_exn proposals |> Dave.Uint8_data.of_string in
    Dave.Session.process_proposals t.session ~proposals ~recognized_user_ids
  in
  (* If process_proposals returns non-empty data, send commit_welcome *)
  let commit_welcome = Dave.Uint8_data.to_string commit_welcome in
  if String.is_empty commit_welcome
  then [%log.debug [%here] "process_proposals returned empty, no commit created"]
  else (
    [%log.debug [%here] "Created commit_welcome"];
    let commit_welcome = Base64.encode_string commit_welcome in
    send_outgoing t (Mls_commit_welcome { commit_welcome }))
;;

let on_mls_prepare_commit_transition
  t
  { Model.Voice_gateway.Event.Mls_prepare_commit_transition.transition_id; commit }
  =
  let%with () = run_if_not_closed t in
  [%log.debug [%here] "Received MLS prepare commit transition" (transition_id : int)];
  let commit_data = Base64.decode_exn commit |> Dave.Uint8_data.of_string in
  let commit_result = Dave.Session.process_commit t.session commit_data in
  let protocol_version = Dave.Session.get_protocol_version t.session in
  let is_ignored = Dave.Commit_result.is_ignored commit_result in
  let is_failed = Dave.Commit_result.is_failed commit_result in
  [%log.debug [%here] "process_commit returned" (is_ignored : bool) (is_failed : bool)];
  if is_ignored
  then [%log.debug [%here] "MLS commit was ignored" (transition_id : int)]
  else if is_failed
  then (
    [%log.error [%here] "MLS commit failed" (transition_id : int)];
    flag_mls_invalid_commit_welcome t ~transition_id;
    handle_dave_protocol_init t ~protocol_version)
  else (
    prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
    maybe_send_dave_protocol_ready_for_transition t ~transition_id);
  Dave.Commit_result.destroy commit_result
;;

let on_mls_welcome t { Model.Voice_gateway.Event.Mls_welcome.transition_id; welcome } =
  let%with () = run_if_not_closed t in
  let recognized_user_ids = Hash_set.to_list t.recognized_user_ids in
  let num_users = List.length recognized_user_ids in
  [%log.debug [%here] "Received MLS welcome" (transition_id : int) (num_users : int)];
  let welcome_result =
    let welcome_data = Base64.decode_exn welcome |> Dave.Uint8_data.of_string in
    Dave.Session.process_welcome t.session welcome_data ~recognized_user_ids
  in
  (* Check if we joined the group by looking at roster member IDs *)
  let roster_count =
    Dave.Welcome_result.get_roster_member_ids welcome_result
    |> Dave.Uint64_data.len
    |> Unsigned.Size_t.to_int
  in
  let joined_group = roster_count > 0 in
  [%log.debug
    [%here]
      "process_welcome result"
      (transition_id : int)
      (roster_count : int)
      (joined_group : bool)];
  if joined_group
  then (
    let protocol_version = Dave.Session.get_protocol_version t.session in
    [%log.debug [%here] "Successfully joined group via welcome" (protocol_version : int)];
    prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
    maybe_send_dave_protocol_ready_for_transition t ~transition_id)
  else (
    (* Welcome didn't result in joining group - flag invalid and send new key package *)
    [%log.debug
      [%here] "MLS welcome didn't result in joining group" (transition_id : int)];
    flag_mls_invalid_commit_welcome t ~transition_id;
    send_mls_key_package t);
  Dave.Welcome_result.destroy welcome_result
;;

let encrypt t ~ssrc ~frame ~output =
  let%with () =
    run_if_not_closed' t ~default:(Dave.Encryptor_result_code.Missing_cryptor, 0)
  in
  Dave.Encryptor.encrypt
    t.encryptor
    ~media_type:Audio
    ~ssrc:(Model.Ssrc.to_int_exn ssrc)
    ~frame
    ~output
;;

let get_max_ciphertext_byte_size t ~frame_size =
  let%with () = run_if_not_closed' t ~default:0 in
  Dave.Encryptor.get_max_ciphertext_byte_size t.encryptor ~media_type:Audio ~frame_size
;;

let decrypt t ~encrypted_frame ~output =
  let%with () =
    run_if_not_closed' t ~default:(Dave.Decryptor_result_code.Missing_cryptor, 0)
  in
  Dave.Decryptor.decrypt t.decryptor ~media_type:Audio ~encrypted_frame ~output
;;

let get_max_plaintext_byte_size t ~encrypted_frame_size =
  let%with () = run_if_not_closed' t ~default:0 in
  Dave.Decryptor.get_max_plaintext_byte_size
    t.decryptor
    ~media_type:Audio
    ~encrypted_frame_size
;;

let assign_ssrc_to_codec t ~ssrc ~codec =
  let%with () = run_if_not_closed t in
  Dave.Encryptor.assign_ssrc_to_codec
    t.encryptor
    ~ssrc:(Model.Ssrc.to_int_exn ssrc)
    ~codec
;;
