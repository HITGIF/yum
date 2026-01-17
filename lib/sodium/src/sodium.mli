open! Core

(** [init] must be called before any other function. It is safe to call multiple times.
    Returns [Error] if the library couldn't be initialized and it is not safe to use. *)
val init : unit -> unit Or_error.t

module Aead_xchacha20poly1305_ietf : sig
  val nonce_len : int
  val nonce : unit -> bytes

  val encrypt
    :  ?additional_data:bytes
    -> message:bytes
    -> nonce:bytes
    -> key:bytes
    -> unit
    -> bytes Or_error.t

  val decrypt
    :  ?additional_data:bytes
    -> ciphertext:bytes
    -> nonce:bytes
    -> key:bytes
    -> unit
    -> bytes Or_error.t
end
