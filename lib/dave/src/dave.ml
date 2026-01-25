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

  val ptr : t -> Element.t ptr
  val len : t -> size_t
  val is_empty : t -> bool
  val of_ptr : (Element.t ptr ptr -> size_t ptr -> unit) -> t
  val of_list : Element.t list -> t
  val of_carray : Element.t carray -> t
  val to_carray : t -> Element.t carray
end = struct
  type t = Element.t carray

  let ptr = CArray.start
  let len t = CArray.length t |> to_size_t
  let is_empty t = CArray.length t = 0

  let of_ptr f =
    let data = allocate (Ctypes.ptr Element.t) (from_voidp Element.t null) in
    let length = allocate size_t (to_size_t 0) in
    f data length;
    CArray.from_ptr !@data (of_size_t !@length)
  ;;

  let of_list = CArray.of_list Element.t
  let of_carray = Fn.id
  let to_carray = Fn.id
end

module Uint8_data = struct
  include Data (struct
      type t = UInt8.t

      let t = uint8_t
    end)

  let to_string t =
    let arr = to_carray t in
    let len = CArray.length arr in
    let bytes = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set bytes i (CArray.get arr i |> UInt8.to_int |> Char.of_int_exn)
    done;
    Bytes.to_string bytes
  ;;

  let of_string string =
    let len = String.length string in
    let arr = CArray.make uint8_t len in
    for i = 0 to len - 1 do
      CArray.set arr i (String.get string i |> Char.to_int |> UInt8.of_int)
    done;
    of_carray arr
  ;;
end

module Uint64_data = Data (struct
    type t = UInt64.t

    let t = uint64_t
  end)

module String_data = struct
  include Data (struct
      type t = char ptr

      let t = ptr char
    end)

  let of_list strings =
    List.map strings ~f:(Fn.compose CArray.start CArray.of_string) |> of_list
  ;;
end

module Commit_result = struct
  open! Binding.Commit_result

  type nonrec t = t

  let destroy = destroy
  let is_failed = is_failed
  let is_ignored = is_ignored
end

module Welcome_result = struct
  open! Binding.Welcome_result

  type nonrec t = t

  let destroy = destroy
  let get_roster_member_ids t = get_roster_member_ids t |> Uint64_data.of_ptr
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

  let destroy = destroy

  let init t ~version ~group_id ~self_user_id =
    init t (to_u16 version) (to_u64 group_id) self_user_id
  ;;

  let reset = reset
  let set_protocol_version t ~version = set_protocol_version t (to_u16 version)
  let get_protocol_version t = get_protocol_version t |> of_u16

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
    |> Uint8_data.of_ptr
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

  let get_marshalled_key_package t = get_marshalled_key_package t |> Uint8_data.of_ptr
  let get_key_ratchet = get_key_ratchet
end

module Codec = Binding.Codec
module Media_type = Binding.Media_type

module Encryptor = struct
  open! Binding.Encryptor

  module Result_code = struct
    type t = Result_code.t =
      | Success
      | Encryption_failure
      | Missing_key_ratchet
      | Missing_cryptor
      | Too_many_attempts
    [@@deriving sexp_of]
  end

  type nonrec t = t

  let create = create
  let destroy = destroy
  let set_key_ratchet = set_key_ratchet
  let set_passthrough_mode = set_passthrough_mode
  let assign_ssrc_to_codec t ~ssrc ~codec = assign_ssrc_to_codec t (to_u32 ssrc) codec
  let get_protocol_version t = get_protocol_version t |> of_u16
  let has_key_ratchet = has_key_ratchet
  let is_passthrough_mode = is_passthrough_mode

  let get_max_ciphertext_byte_size t ~media_type ~plaintext_len =
    get_max_ciphertext_byte_size t media_type (to_size_t plaintext_len) |> of_size_t
  ;;

  let encrypt t ~media_type ~ssrc ~plaintext =
    let plaintext_len = Bytes.length plaintext in
    let ciphertext =
      Bytes.create (get_max_ciphertext_byte_size t ~media_type ~plaintext_len)
    in
    let ciphertext_len = allocate size_t (to_size_t 0) in
    let result =
      encrypt
        t
        media_type
        (to_u32 ssrc)
        !!plaintext
        (to_size_t plaintext_len)
        !!ciphertext
        (to_size_t (Bytes.length ciphertext))
        ciphertext_len
    in
    let ciphertext = Bytes.subo ciphertext ~len:(of_size_t !@ciphertext_len) in
    result, ciphertext
  ;;
end

module Decryptor = struct
  open! Binding.Decryptor

  module Result_code = struct
    type t = Result_code.t =
      | Success
      | Decryption_failure
      | Missing_key_ratchet
      | Invalid_nonce
      | Missing_cryptor
    [@@deriving sexp_of]
  end

  type nonrec t = t

  let create = create
  let destroy = destroy
  let transition_to_key_ratchet = transition_to_key_ratchet
  let set_passthrough_mode = set_passthrough_mode

  let get_max_plaintext_byte_size t ~media_type ~ciphertext_len =
    get_max_plaintext_byte_size t media_type (to_size_t ciphertext_len) |> of_size_t
  ;;

  let decrypt t ~media_type ~ciphertext =
    let ciphertext_len = Bytes.length ciphertext in
    let plaintext =
      Bytes.create (get_max_plaintext_byte_size t ~media_type ~ciphertext_len)
    in
    let plaintext_len = allocate size_t (to_size_t 0) in
    let result =
      decrypt
        t
        media_type
        !!ciphertext
        (to_size_t ciphertext_len)
        !!plaintext
        (to_size_t (Bytes.length plaintext))
        plaintext_len
    in
    let plaintext = Bytes.subo plaintext ~len:(of_size_t !@plaintext_len) in
    result, plaintext
  ;;
end

module Logging = struct
  open! Binding.Logging

  let () =
    set_log_sink_callback (fun severity file line message ->
      let level =
        match severity with
        | Verbose | Info -> Some `Debug
        | Warning | Error -> Some `Error
        | None -> None
      in
      match level with
      | None -> ()
      | Some level ->
        let filename =
          match String.rsplit2 ~on:'/' file with
          | Some (_, filename) -> filename
          | None -> file
        in
        Async_log.printf
          ~level
          (force Async_log.Global.log)
          "[libdave] [%s:%d] %s"
          filename
          line
          message)
  ;;
end
