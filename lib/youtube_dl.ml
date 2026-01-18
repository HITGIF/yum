open! Core
open! Async
open! Common

let default_prog = File_path.Absolute.of_string "/usr/bin/youtube-dl"

let default_download_args =
  [ "--quiet"; "--no-warnings"; "--no-progress"; "--no-continue" ]
  @ List.concat
      [ [ "--format"; "bestaudio" ]
      ; [ "--user-agent"
        ; "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) \
           Chrome/127.0.6533.103 Mobile Safari/537.36"
          (* https://github.com/ytdl-org/youtube-dl/issues/33142 *)
        ]
      ; [ "--output"; "-" ]
      ]
;;

let default_get_playlist_args = [ "--get-id"; "--flat-playlist" ]

let download
  ?cancellation_token
  ?(prog = default_prog)
  ?(args = default_download_args)
  url
  =
  let args = args @ [ url ] in
  Stream_process.stream ?cancellation_token ~prog ~args ()
;;

let get_playlist ?(prog = default_prog) ?(args = default_get_playlist_args) url =
  let args = args @ [ url ] in
  let%map.Deferred.Or_error songs =
    Process.run ~prog:(File_path.Absolute.to_string prog) ~args ()
  in
  String.split_lines songs |> List.map ~f:Song.of_youtube_string
;;
