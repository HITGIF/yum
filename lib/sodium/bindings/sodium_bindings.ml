open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  let sodium_init = foreign "sodium_init" (void @-> returning int)

  module Random = struct
    let bytes = foreign "randombytes_buf" (ocaml_bytes @-> size_t @-> returning void)
  end

  (** https://libsodium.gitbook.io/doc/secret-key_cryptography/aead/chacha20-poly1305/xchacha20-poly1305_construction *)
  module Aead_xchacha20poly1305_ietf = struct
    let auth_len =
      foreign "crypto_aead_xchacha20poly1305_ietf_abytes" (void @-> returning size_t)
    ;;

    let key_len =
      foreign "crypto_aead_xchacha20poly1305_ietf_keybytes" (void @-> returning size_t)
    ;;

    let nonce_len =
      foreign "crypto_aead_xchacha20poly1305_ietf_npubbytes" (void @-> returning size_t)
    ;;

    let encrypt =
      foreign
        "crypto_aead_xchacha20poly1305_ietf_encrypt"
        (ocaml_bytes (* ciphertext [c] *)
         @-> ptr ullong (* ciphertext len [clen_p] *)
         @-> ocaml_bytes (* message [m] *)
         @-> ullong (* message len [mlen] *)
         @-> ocaml_bytes (* additional data [ad] *)
         @-> ullong (* additional data len [adlen] *)
         @-> ptr void (* NULL [nsec] *)
         @-> ocaml_bytes (* nonce [npub] *)
         @-> ocaml_bytes (* key [k] *)
         @-> returning int)
    ;;

    let decrypt =
      foreign
        "crypto_aead_xchacha20poly1305_ietf_decrypt"
        (ocaml_bytes (* message [c] *)
         @-> ptr ullong (* message len [clen_p] *)
         @-> ptr void (* NULL [nsec] *)
         @-> ocaml_bytes (* ciphertext [m] *)
         @-> ullong (* ciphertext len [mlen] *)
         @-> ocaml_bytes (* additional data [ad] *)
         @-> ullong (* additional data len [adlen] *)
         @-> ocaml_bytes (* nonce [npub] *)
         @-> ocaml_bytes (* key [k] *)
         @-> returning int)
    ;;
  end
end
