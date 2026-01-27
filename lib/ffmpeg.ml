open! Core
open! Async
open! Common

let default_prog = File_path.Absolute.of_string "/usr/bin/ffmpeg"

let args ?(bitrate = "128k") () =
  let format = Audio.Pcm_frame.ffmpeg_format in
  let channels = Audio.Pcm_frame.channels |> Int.to_string in
  let sample_rate = Audio.Pcm_frame.sample_rate |> Int.to_string in
  List.concat
    [ [ "-i"; "pipe:0" ] (* input *)
    ; [ "-f"; format ] (* format *)
    ; [ "-ac"; channels ] (* audio channels *)
    ; [ "-ar"; sample_rate ] (* audio sample rate *)
    ; [ "-b:a"; bitrate ] (* audio bitrate *)
    ; [ "-loglevel"; "panic" ] (* log level *)
    ; [ "-hide_banner" ]
    ; [ "pipe:1" ] (* output *)
    ]
;;

let encode_pcm ?cancellation_token ?on_finish ?(prog = default_prog) ?bitrate stdin =
  let args = args ?bitrate () in
  Stream_process.stream ?cancellation_token ?on_finish ~prog ~args ~stdin ()
  >>|? Audio.Pcm_frame.read
;;
