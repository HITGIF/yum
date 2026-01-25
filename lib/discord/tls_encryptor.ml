open! Core

type t = [ `Aead_xchacha20_poly1305_rtpsize ] [@@deriving enumerate, sexp_of]

let mode t = (t :> Model.Tls_encryption_mode.t)

let create modes =
  match
    List.find [%all: t] ~f:(fun mode ->
      List.mem
        modes
        (mode :> Model.Tls_encryption_mode.t)
        ~equal:[%equal: Model.Tls_encryption_mode.t])
  with
  | Some mode -> Ok mode
  | None ->
    Or_error.error_s
      [%message
        "No supported encrypyion modes"
          ~supported:([%all: t] : t list)
          ~available:(modes : Model.Tls_encryption_mode.t list)]
;;

let pad_nonce int ~len =
  let buf = Bytes.make len '\000' |> Iobuf.of_bytes in
  Iobuf.Poke.uint32_be_trunc buf ~pos:0 int;
  Iobuf.to_bytes buf
;;

let encrypt ~header ~frame ~secret_key ~nonce = function
  | `Aead_xchacha20_poly1305_rtpsize ->
    (* https://github.com/Rapptz/discord.py/blob/005287898393bd13d21e077e8607d5226757820a/discord/voice_client.py#L384 *)
    let ciphertext =
      Sodium.Aead_xchacha20poly1305_ietf.encrypt
        ~message:frame
        ~additional_data:header
        ~nonce:(pad_nonce nonce ~len:Sodium.Aead_xchacha20poly1305_ietf.nonce_len)
        ~key:secret_key
        ()
      |> ok_exn
    in
    let nonce = pad_nonce nonce ~len:4 in
    let buf =
      Iobuf.create ~len:Bytes.(length header + length ciphertext + length nonce)
    in
    Iobuf.Fill.byteso buf header;
    Iobuf.Fill.byteso buf ciphertext;
    Iobuf.Fill.byteso buf nonce;
    Iobuf.flip_lo buf;
    Iobuf.read_only buf
;;

module%test _ = struct
  let print_bytes_hex bytes =
    Bytes.to_list bytes |> List.iter ~f:(fun b -> printf "%02x" (Char.to_int b))
  ;;

  let%expect_test "nonce" =
    let pad_nonce = pad_nonce ~len:Sodium.Aead_xchacha20poly1305_ietf.nonce_len in
    print_bytes_hex (pad_nonce 12);
    [%expect {| 0000000c0000000000000000000000000000000000000000 |}];
    print_bytes_hex (pad_nonce Unsigned.UInt32.(max_int |> to_int));
    [%expect {| ffffffff0000000000000000000000000000000000000000 |}];
    print_bytes_hex (pad_nonce Unsigned.UInt32.(max_int |> to_int |> ( + ) 1));
    [%expect {| 000000000000000000000000000000000000000000000000 |}];
    print_bytes_hex (pad_nonce Unsigned.UInt32.(max_int |> to_int |> ( + ) 1 |> ( + ) 1));
    [%expect {| 000000010000000000000000000000000000000000000000 |}]
  ;;

  let%expect_test "nonce" =
    let pad_nonce = pad_nonce ~len:4 in
    print_bytes_hex (pad_nonce 12);
    [%expect {| 0000000c |}];
    print_bytes_hex (pad_nonce Unsigned.UInt32.(max_int |> to_int));
    [%expect {| ffffffff |}];
    print_bytes_hex (pad_nonce Unsigned.UInt32.(max_int |> to_int |> ( + ) 1));
    [%expect {| 00000000 |}];
    print_bytes_hex (pad_nonce Unsigned.UInt32.(max_int |> to_int |> ( + ) 1 |> ( + ) 1));
    [%expect {| 00000001 |}]
  ;;
end
