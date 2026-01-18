open! Core
open! Async

let channels = 2
let frame_duration = Time_ns.Span.of_int_ms 20
let sample_rate = 48000
let sample_length = Byte_units.of_bytes_int 2 (* S16LE => 16 bits *)
let ffmpeg_format = "s16le"

let samples_per_frame =
  Float.of_int sample_rate *. Time_ns.Span.to_sec frame_duration |> Float.to_int
;;

let frame_length =
  Byte_units.(
    scale (scale sample_length (Float.of_int samples_per_frame)) (Float.of_int channels))
;;

type t = bytes

let read reader =
  let len = Byte_units.bytes_int_exn frame_length in
  Reader.read_all reader (fun reader ->
    let buf = Bytes.create len in
    match%map Reader.really_read ~len reader buf with
    | `Ok -> `Ok buf
    | `Eof _ -> `Eof)
;;

let to_bytes = Fn.id

module%test _ = struct
  let%expect_test "" =
    let frame_length_bytes = Byte_units.bytes_int_exn frame_length in
    print_s [%sexp { samples_per_frame : int; frame_length_bytes : int }];
    [%expect {| ((samples_per_frame 960) (frame_length_bytes 3840)) |}];
    return ()
  ;;
end
