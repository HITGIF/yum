open! Core
open! Ctypes
open! Bindings_core
open! Unsigned
module Binding = Dave_bindings.C (Dave_generated)

let max_supported_protocol_version = Binding.max_supported_protocol_version ()

module Data (Element : sig
    type t

    val t : t typ
  end) : sig
  type t

  val get : (Element.t ptr ptr -> size_t ptr -> unit) -> t
  val of_list : Element.t list -> t
  val ptr : t -> Element.t ptr
  val len : t -> size_t
end = struct
  type t = Element.t carray

  let create ptr len = CArray.from_ptr ptr (of_size_t len)

  let get f =
    let data = allocate (ptr Element.t) (from_voidp Element.t null) in
    let length = allocate size_t (to_size_t 0) in
    f data length;
    create !@data !@length
  ;;

  let of_list = CArray.of_list Element.t
  let ptr = CArray.start
  let len t = CArray.length t |> to_size_t
end

module Uint8_data = Data (struct
    type t = UInt8.t

    let t = uint8_t
  end)

module Uint64_data = Data (struct
    type t = UInt64.t

    let t = uint64_t
  end)

module String_data = struct
  include Data (struct
      type t = string ocaml

      let t = ocaml_string
    end)

  let of_list strings = List.map strings ~f:ocaml_string_start |> of_list
end

module Commit_result = struct
  open! Binding.Commit_result

  type nonrec t = t

  let destroy = destroy
  let is_failed = is_failed
  let is_ignored = is_ignored
  let get_roster_member_ids t = get_roster_member_ids t |> Uint64_data.get

  let get_roster_member_signature t roster_id =
    get_roster_member_signature t (to_u64 roster_id) |> Uint8_data.get
  ;;
end

module Welcome_result = struct
  open! Binding.Welcome_result

  type nonrec t = t

  let destroy = destroy
  let get_roster_member_ids t = get_roster_member_ids t |> Uint64_data.get

  let get_roster_member_signature t roster_id =
    get_roster_member_signature t (to_u64 roster_id) |> Uint8_data.get
  ;;
end

module Key_ratchet = struct
  open! Binding.Key_ratchet

  type nonrec t = t

  let destroy = destroy
end

module Session = struct
  open! Binding.Session

  type nonrec t = t

  let create ~on_error =
    let on_error source reason _user_data = on_error ~source ~reason in
    create null null on_error null
  ;;

  let init t ~version ~group_id ~self_user_id =
    init t (to_u16 version) (to_u64 group_id) self_user_id
  ;;

  let reset = reset
  let set_protocol_version t ~version = set_protocol_version t (to_u64 version)
  let get_protocol_version t = get_protocol_version t |> of_u64
  let get_last_epoch_authenticator t = get_last_epoch_authenticator t |> Uint8_data.get

  let set_external_sender t external_sender =
    set_external_sender
      t
      (Uint8_data.ptr external_sender)
      (Uint8_data.len external_sender)
  ;;

  let process_proposals t ~proposals ~recognized_user_ids =
    let recognized_user_ids = String_data.of_list recognized_user_ids in
    process_proposals
      t
      (Uint8_data.ptr proposals)
      (Uint8_data.len proposals)
      (String_data.ptr recognized_user_ids)
      (String_data.len recognized_user_ids)
    |> Uint8_data.get
  ;;

  let process_commit t commit =
    process_commit t (Uint8_data.ptr commit) (Uint8_data.len commit)
  ;;

  let process_welcome t welcome ~recognized_user_ids =
    let recognized_user_ids = String_data.of_list recognized_user_ids in
    process_welcome
      t
      (Uint8_data.ptr welcome)
      (Uint8_data.len welcome)
      (String_data.ptr recognized_user_ids)
      (String_data.len recognized_user_ids)
  ;;

  let get_marshalled_key_package t = get_marshalled_key_package t |> Uint8_data.get
  let get_key_ratchet = get_key_ratchet
end
