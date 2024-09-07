open! Core

type t = Youtube of string [@@deriving variants]

let supported_url_formats_msg =
  [ "> Supported `<url>` formats:"; "> - `https://www.youtube.com/watch?v=[...]`" ]
  |> String.concat ~sep:"\n"
;;

let youtube_prefix = "https://www.youtube.com/watch?v="
let of_string = youtube

let to_url = function
  | Youtube id -> [%string "%{youtube_prefix}%{id}"]
;;

let of_url t =
  String.chop_prefix ~prefix:youtube_prefix t
  |> function
  | Some id -> Ok (Youtube id)
  | None ->
    Or_error.error_string
      [%string "URL format is not supported.\n\n%{supported_url_formats_msg}"]
;;
