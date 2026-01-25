open! Core

type t [@@deriving sexp_of]

val create : Model.Tls_encryption_mode.t list -> t Or_error.t
val mode : t -> Model.Tls_encryption_mode.t

val encrypt
  :  header:bytes
  -> frame:bytes
  -> secret_key:bytes
  -> nonce:int
  -> t
  -> (read, Iobuf.seek, Iobuf.global) Iobuf.t
