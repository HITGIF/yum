open! Core
open! Ctypes
open! Bindings_core
module Binding = Sodium_bindings.C (Sodium_generated)

let init () =
  match Binding.sodium_init () with
  | 0 (* Initialized for the first time *) | 1 (* Already initialized *) -> Ok ()
  | errno ->
    Or_error.error_s [%message [%here] "libsodium intialization failed" (errno : int)]
;;

module Aead_xchacha20poly1305_ietf = struct
  open Binding.Aead_xchacha20poly1305_ietf

  let auth_len = of_size_t (auth_len ())
  let key_len = of_size_t (key_len ())
  let nonce_len = of_size_t (nonce_len ())

  let nonce () =
    let ret = Bytes.create nonce_len in
    Binding.Random.bytes !!ret (to_size_t nonce_len);
    ret
  ;;

  let encrypt ?additional_data ~message ~nonce ~key () =
    let%bind.Or_error () = validate_len key ~expect:key_len in
    let%bind.Or_error () = validate_len nonce ~expect:nonce_len in
    let ciphertext = Bytes.create (Bytes.length message + auth_len) in
    let ciphertext_len = allocate ullong (to_ull 0) in
    match
      encrypt
        !!ciphertext
        ciphertext_len
        !!message
        !?message
        !!?additional_data
        !??additional_data
        null
        !!nonce
        !!key
    with
    | 0 -> Bytes.subo ciphertext ~len:(of_ull !@ciphertext_len) |> Ok
    | errno ->
      Or_error.error_s [%message [%here] [%module_name] "encryption failed" (errno : int)]
  ;;

  let decrypt ?additional_data ~ciphertext ~nonce ~key () =
    let%bind.Or_error () = validate_len key ~expect:key_len in
    let%bind.Or_error () = validate_len nonce ~expect:nonce_len in
    let message = Bytes.create (Bytes.length ciphertext - auth_len) in
    let message_len = allocate ullong (to_ull 0) in
    match
      decrypt
        !!message
        message_len
        null
        !!ciphertext
        !?ciphertext
        !!?additional_data
        !??additional_data
        !!nonce
        !!key
    with
    | 0 -> Bytes.subo message ~len:(of_ull !@message_len) |> Ok
    | errno ->
      Or_error.error_s [%message [%here] [%module_name] "decryption failed" (errno : int)]
  ;;
end
