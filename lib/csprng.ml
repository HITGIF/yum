let random_string len = Mirage_crypto_rng.generate len

let random_2bytes_int () =
  let src = Mirage_crypto_rng.generate 2 in
  (String.get_int8 src 0 * 256) + String.get_int8 src 1
;;

let random_4bytes_int () =
  let src = Mirage_crypto_rng.generate 4 in
  (String.get_int8 src 0 * 256 * 256 * 256)
  + (String.get_int8 src 1 * 256 * 256)
  + (String.get_int8 src 2 * 256)
  + String.get_int8 src 3
;;
