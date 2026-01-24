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
  { mls_session : Dave.Session.t
  ; self_user_id : string
  ; group_id : int
  ; mutable recognized_user_ids : String.Set.t
  ; protocol_transitions : int Int.Table.t
  ; mutable latest_prepared_transition_version : int
  ; outgoing_writer : Outgoing.t Pipe.Writer.t
  ; outgoing_reader : Outgoing.t Pipe.Reader.t
  ; encryptor : Dave.Encryptor.t
  ; decryptor : Dave.Decryptor.t
  }
[@@deriving fields ~getters]

let create ~self_user_id ~group_id =
  let on_error ~source ~reason =
    [%log.error [%here] "MLS failure" (source : string) (reason : string)]
  in
  let mls_session = Dave.Session.create ~on_error in
  let outgoing_reader, outgoing_writer = Pipe.create () in
  let encryptor = Dave.Encryptor.create () in
  let decryptor = Dave.Decryptor.create () in
  (* Start in passthrough mode until MLS handshake completes *)
  Dave.Encryptor.set_passthrough_mode encryptor true;
  Dave.Decryptor.set_passthrough_mode decryptor true;
  { mls_session
  ; self_user_id
  ; group_id
  ; recognized_user_ids = String.Set.empty
  ; protocol_transitions = Int.Table.create ()
  ; latest_prepared_transition_version = 0
  ; outgoing_writer
  ; outgoing_reader
  ; encryptor
  ; decryptor
  }
;;

let outgoing t = t.outgoing_reader
let send_outgoing t msg = Pipe.write_without_pushback_if_open t.outgoing_writer msg
let get_recognized_user_ids t = Set.to_list t.recognized_user_ids @ [ t.self_user_id ]

let make_user_key_ratchet t ~user_id ~protocol_version =
  if protocol_version = disabled_version
  then None
  else Some (Dave.Session.get_key_ratchet t.mls_session user_id)
;;

let setup_key_ratchet_for_user t ~user_id ~protocol_version =
  let key_ratchet = make_user_key_ratchet t ~user_id ~protocol_version in
  (* Update encryptor/decryptor if this is for self *)
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
  let recognized_users = get_recognized_user_ids t in
  [%log.debug
    [%here]
      "Preparing DAVE protocol ratchets"
      (transition_id : int)
      (protocol_version : int)
      (recognized_users : string list)];
  (* Setup key ratchets for all recognized users except self *)
  List.iter recognized_users ~f:(fun user_id ->
    if not (String.equal user_id t.self_user_id)
    then setup_key_ratchet_for_user t ~user_id ~protocol_version);
  (* For init transition, also setup self immediately; otherwise defer to execute *)
  if transition_id = init_transition_id
  then setup_key_ratchet_for_user t ~user_id:t.self_user_id ~protocol_version
  else Hashtbl.set t.protocol_transitions ~key:transition_id ~data:protocol_version;
  t.latest_prepared_transition_version <- protocol_version
;;

let maybe_send_dave_protocol_ready_for_transition t ~transition_id =
  if transition_id <> init_transition_id
  then send_outgoing t (Dave_protocol_ready_for_transition { transition_id })
;;

let send_mls_key_package t =
  let key_package = Dave.Session.get_marshalled_key_package t.mls_session in
  let key_package_str = Dave.Uint8_data.to_string key_package in
  let key_package_b64 = Base64.encode_string key_package_str in
  [%log.debug [%here] "Sending MLS key package"];
  send_outgoing t (Mls_key_package { key_package = key_package_b64 })
;;

let handle_dave_protocol_prepare_epoch t ~epoch ~protocol_version ~group_id =
  if epoch = mls_new_group_expected_epoch
  then
    Dave.Session.init
      t.mls_session
      ~version:protocol_version
      ~group_id
      ~self_user_id:t.self_user_id
;;

let handle_dave_protocol_execute_transition t ~transition_id =
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
    if protocol_version = disabled_version then Dave.Session.reset t.mls_session;
    setup_key_ratchet_for_user t ~user_id:t.self_user_id ~protocol_version
;;

let flag_mls_invalid_commit_welcome t ~transition_id =
  send_outgoing t (Mls_invalid_commit_welcome { transition_id })
;;

let handle_dave_protocol_init t ~protocol_version =
  if protocol_version > 0
  then (
    handle_dave_protocol_prepare_epoch
      t
      ~epoch:mls_new_group_expected_epoch
      ~protocol_version
      ~group_id:t.group_id;
    send_mls_key_package t)
  else (
    prepare_dave_protocol_ratchets t ~transition_id:init_transition_id ~protocol_version;
    handle_dave_protocol_execute_transition t ~transition_id:init_transition_id)
;;

(** Add an allowed user to the connection *)
let create_user t ~user_id =
  let already_exists = Set.mem t.recognized_user_ids user_id in
  t.recognized_user_ids <- Set.add t.recognized_user_ids user_id;
  let count = Set.length t.recognized_user_ids in
  [%log.debug
    [%here]
      "Adding recognized user"
      (user_id : string)
      (already_exists : bool)
      (count : int)];
  setup_key_ratchet_for_user
    t
    ~user_id
    ~protocol_version:t.latest_prepared_transition_version
;;

(** Remove an allowed user from the connection *)
let destroy_user t ~user_id =
  let existed = Set.mem t.recognized_user_ids user_id in
  t.recognized_user_ids <- Set.remove t.recognized_user_ids user_id;
  let count = Set.length t.recognized_user_ids in
  [%log.debug
    [%here] "Removing recognized user" (user_id : string) (existed : bool) (count : int)]
;;

(** Handle Session_description (opcode 4) - called when dave_protocol_version is set *)
let on_session_description t ~dave_protocol_version =
  handle_dave_protocol_init t ~protocol_version:dave_protocol_version
;;

(** Handle Dave_protocol_prepare_transition (opcode 21) *)
let on_dave_protocol_prepare_transition t ~transition_id ~protocol_version =
  prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
  maybe_send_dave_protocol_ready_for_transition t ~transition_id
;;

(** Handle Dave_protocol_execute_transition (opcode 22) *)
let on_dave_protocol_execute_transition t ~transition_id =
  handle_dave_protocol_execute_transition t ~transition_id
;;

(** Handle Dave_protocol_prepare_epoch (opcode 24) *)
let on_dave_protocol_prepare_epoch t ~epoch ~protocol_version =
  handle_dave_protocol_prepare_epoch t ~epoch ~protocol_version ~group_id:t.group_id;
  if Int.equal epoch mls_new_group_expected_epoch then send_mls_key_package t
;;

(** Handle Mls_external_sender_package (opcode 25) *)
let on_mls_external_sender_package t ~external_sender_package =
  [%log.debug [%here] "Received MLS external sender package"];
  let data = Base64.decode_exn external_sender_package |> Dave.Uint8_data.of_string in
  Dave.Session.set_external_sender t.mls_session data
;;

(** Handle Mls_proposals (opcode 27) *)
let on_mls_proposals t ~proposals =
  let recognized_user_ids = Set.to_list t.recognized_user_ids in
  let recognized_count = List.length recognized_user_ids in
  [%log.debug [%here] "Received MLS proposals" (recognized_count : int)];
  let proposals_data = Base64.decode_exn proposals |> Dave.Uint8_data.of_string in
  let commit_welcome =
    Dave.Session.process_proposals
      t.mls_session
      ~proposals:proposals_data
      ~recognized_user_ids
  in
  (* If process_proposals returns non-empty data, we need to send commit_welcome *)
  let commit_welcome_str = Dave.Uint8_data.to_string commit_welcome in
  let commit_welcome_len = String.length commit_welcome_str in
  if commit_welcome_len > 0
  then (
    [%log.debug [%here] "Created commit_welcome" (commit_welcome_len : int)];
    let commit_welcome_b64 = Base64.encode_string commit_welcome_str in
    send_outgoing t (Mls_commit_welcome { commit_welcome = commit_welcome_b64 }))
  else [%log.debug [%here] "process_proposals returned empty, no commit created"]
;;

(** Handle Mls_prepare_commit_transition (opcode 29) *)
let on_mls_prepare_commit_transition t ~transition_id ~commit =
  [%log.debug [%here] "Received MLS prepare commit transition" (transition_id : int)];
  let commit_data = Base64.decode_exn commit |> Dave.Uint8_data.of_string in
  let result = Dave.Session.process_commit t.mls_session commit_data in
  let is_ignored = Dave.Commit_result.is_ignored result in
  let is_failed = Dave.Commit_result.is_failed result in
  [%log.debug [%here] "process_commit returned" (is_ignored : bool) (is_failed : bool)];
  if is_ignored
  then [%log.debug [%here] "MLS commit was ignored by library" (transition_id : int)]
  else if is_failed
  then (
    (* Commit failed - reset and reinit.
       Use latest_prepared_transition_version since get_protocol_version returns 0 after reset. *)
    [%log.debug [%here] "MLS commit failed, resetting" (transition_id : int)];
    flag_mls_invalid_commit_welcome t ~transition_id;
    let protocol_version = t.latest_prepared_transition_version in
    Dave.Session.reset t.mls_session;
    handle_dave_protocol_init t ~protocol_version)
  else (
    (* Someone else's commit - check if we joined the group *)
    let roster_ids = Dave.Commit_result.get_roster_member_ids result in
    let roster_count = Dave.Uint64_data.len roster_ids |> Unsigned.Size_t.to_int in
    let joined_group = roster_count > 0 in
    [%log.debug
      [%here]
        "process_commit result"
        (transition_id : int)
        (roster_count : int)
        (joined_group : bool)];
    if joined_group
    then (
      let protocol_version = Dave.Session.get_protocol_version t.mls_session in
      prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
      maybe_send_dave_protocol_ready_for_transition t ~transition_id)
    else (
      (* Commit was not ignored but we didn't join - flag invalid and reinit.
         Use latest_prepared_transition_version for consistency. *)
      [%log.debug
        [%here]
          "MLS commit didn't result in joining group, reinitializing"
          (transition_id : int)];
      let protocol_version = t.latest_prepared_transition_version in
      flag_mls_invalid_commit_welcome t ~transition_id;
      handle_dave_protocol_init t ~protocol_version));
  Dave.Commit_result.destroy result
;;

(** Handle Mls_welcome (opcode 30) *)
let on_mls_welcome t ~transition_id ~welcome =
  let recognized_user_ids = Set.to_list t.recognized_user_ids in
  let recognized_count = List.length recognized_user_ids in
  [%log.debug
    [%here] "Received MLS welcome" (transition_id : int) (recognized_count : int)];
  let welcome_data = Base64.decode_exn welcome |> Dave.Uint8_data.of_string in
  let result =
    Dave.Session.process_welcome t.mls_session welcome_data ~recognized_user_ids
  in
  (* Check if we joined the group by looking at roster member IDs *)
  let roster_ids = Dave.Welcome_result.get_roster_member_ids result in
  let roster_count = Dave.Uint64_data.len roster_ids |> Unsigned.Size_t.to_int in
  let joined_group = roster_count > 0 in
  [%log.debug
    [%here]
      "process_welcome result"
      (transition_id : int)
      (roster_count : int)
      (joined_group : bool)];
  if joined_group
  then (
    let protocol_version = Dave.Session.get_protocol_version t.mls_session in
    [%log.debug [%here] "Successfully joined group via welcome" (protocol_version : int)];
    prepare_dave_protocol_ratchets t ~transition_id ~protocol_version;
    maybe_send_dave_protocol_ready_for_transition t ~transition_id)
  else (
    (* Welcome didn't result in joining group - flag invalid and send new key package *)
    [%log.debug
      [%here] "MLS welcome didn't result in joining group" (transition_id : int)];
    flag_mls_invalid_commit_welcome t ~transition_id;
    send_mls_key_package t);
  Dave.Welcome_result.destroy result
;;

(** Encrypt a frame for sending *)
let encrypt t ~ssrc ~frame ~output =
  Dave.Encryptor.encrypt t.encryptor ~media_type:Audio ~ssrc ~frame ~output
;;

(** Get max ciphertext size for a given frame size *)
let get_max_ciphertext_byte_size t ~frame_size =
  Dave.Encryptor.get_max_ciphertext_byte_size t.encryptor ~media_type:Audio ~frame_size
;;

(** Decrypt a received frame *)
let decrypt t ~encrypted_frame ~output =
  Dave.Decryptor.decrypt t.decryptor ~media_type:Audio ~encrypted_frame ~output
;;

(** Get max plaintext size for a given encrypted frame size *)
let get_max_plaintext_byte_size t ~encrypted_frame_size =
  Dave.Decryptor.get_max_plaintext_byte_size
    t.decryptor
    ~media_type:Audio
    ~encrypted_frame_size
;;

(** Assign SSRC to codec for encryption *)
let assign_ssrc_to_codec t ~ssrc ~codec =
  Dave.Encryptor.assign_ssrc_to_codec t.encryptor ~ssrc ~codec
;;

let destroy t =
  Dave.Encryptor.destroy t.encryptor;
  Dave.Decryptor.destroy t.decryptor;
  Pipe.close t.outgoing_writer
;;
