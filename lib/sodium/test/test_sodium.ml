open! Core
open! Sodium
open! Expect_test_helpers_core

module%test [@name "Aead_xchacha20poly1305_ietf"] _ = struct
  open! Aead_xchacha20poly1305_ietf

  let%expect_test "roundtrip" =
    init () |> ok_exn;
    let message = Bytes.of_string "foo" in
    let nonce = nonce () in
    let key = Bytes.of_string "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in
    let ciphertext = encrypt ~message ~nonce ~key () |> ok_exn in
    let message' = decrypt ~ciphertext ~nonce ~key () |> ok_exn in
    print_endline (Bytes.to_string message');
    [%expect {| foo |}]
  ;;

  let%expect_test "nonce_len" =
    [%sexp_of: int] nonce_len |> print_s;
    [%expect {| 24 |}]
  ;;
end
