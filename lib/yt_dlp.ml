open! Core
open! Async
open! Common

let default_prog = File_path.Absolute.of_string "/usr/bin/yt-dlp"

let default_download_args =
  [ "--quiet"; "--no-warnings"; "--no-progress"; "--no-continue" ]
  @ List.concat
      [ [ "--user-agent"
        ; "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) \
           Chrome/127.0.6533.103 Mobile Safari/537.36"
        ]
      ; [ "--output"; "-" ]
      ]
;;

let default_get_playlist_args = [ "--get-id"; "--flat-playlist" ]

let download
  ?cancellation_token
  ?on_finish
  ?(prog = default_prog)
  ?(args = default_download_args)
  url
  =
  let args = args @ [ url ] in
  Stream_process.stream ?cancellation_token ?on_finish ~prog ~args ()
;;

let get_playlist ?(prog = default_prog) ?(args = default_get_playlist_args) url =
  let args = args @ [ url ] in
  let%map.Deferred.Or_error songs =
    Process.run ~prog:(File_path.Absolute.to_string prog) ~args ()
  in
  String.split_lines songs |> List.map ~f:Song.of_youtube_string
;;

module Search_result = struct
  type t =
    { id : string
    ; title : string
    ; uploader : string option
    ; duration : string option
    }
  [@@deriving sexp_of, fields ~getters]

  (* yt-dlp prints "NA" for fields it can't resolve in flat-playlist mode. *)
  let field s =
    match String.strip s with
    | "" | "NA" -> None
    | s -> Some s
  ;;

  let of_line line =
    match String.split line ~on:'\t' with
    | id :: title :: rest ->
      let id = String.strip id in
      if String.is_empty id
      then None
      else (
        let uploader, duration =
          match rest with
          | uploader :: duration :: _ -> field uploader, field duration
          | [ uploader ] -> field uploader, None
          | [] -> None, None
        in
        Some { id; title = String.strip title; uploader; duration })
    | _ -> None
  ;;
end

(* [%(id)s] etc. are yt-dlp output template fields; tab-separated so titles
   containing arbitrary characters stay on a single parsable line. *)
let default_search_args =
  [ "--flat-playlist"
  ; "--print"
  ; "%(id)s\t%(title)s\t%(uploader)s\t%(duration_string)s"
  ]
;;

let search ?(prog = default_prog) ~max_results query =
  let url = [%string "ytsearch%{max_results#Int}:%{query}"] in
  let args = default_search_args @ [ url ] in
  let%map.Deferred.Or_error output =
    Process.run ~prog:(File_path.Absolute.to_string prog) ~args ()
  in
  String.split_lines output |> List.filter_map ~f:Search_result.of_line
;;

module%test _ = struct
  let%expect_test "parse search lines" =
    let test line =
      Search_result.of_line line |> [%sexp_of: Search_result.t option] |> print_s
    in
    test "U7L-3VXAkSA\tlove 2000\tSoraya\t3:42";
    test "abc\tno duration\tChannel\tNA";
    test "def\tno uploader or duration\tNA\tNA";
    test "ghi\ttitle with\ttabs in it\tArtist\t1:00";
    test "";
    [%expect
      {|
      (((id U7L-3VXAkSA) (title "love 2000") (uploader (Soraya)) (duration (3:42))))
      (((id abc) (title "no duration") (uploader (Channel)) (duration ())))
      (((id def) (title "no uploader or duration") (uploader ()) (duration ())))
      (((id ghi) (title "title with") (uploader ("tabs in it"))
        (duration (Artist))))
      ()
      |}];
    return ()
  ;;
end
