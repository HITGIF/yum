open! Core
open Or_error.Let_syntax

type t =
  | Ping
  | Start
  | Stop
  | Skip
  | Help
  | Play of Song.t
  | Play_now of Song.t
  | Play_list of Song.Playlist.t
[@@deriving variants]

module Command = struct
  type nonrec t =
    { names : string list
    ; summary : string
    ; args : string
    ; parse_args : string list -> t Or_error.t
    }

  let command_synopsis_length = 35
  let pad_synopsis = String.pad_right ~len:command_synopsis_length

  let help_text { names; summary; args; _ } =
    let names = String.concat names ~sep:" | " in
    pad_synopsis ("[ " ^ names ^ " ] " ^ args) ^ ": " ^ summary
  ;;
end

let too_few_args = Or_error.error_string "Too few arguments"
let too_many_args = Or_error.error_string "Too many arguments"

let parse_no_arg command = function
  | [] -> Ok command
  | _ -> too_many_args
;;

let parse_single_video command = function
  | [ url ] -> Song.of_url url >>| command
  | [] -> too_few_args
  | _ -> too_many_args
;;

let parse_single_playlist command = function
  | [ url ] -> Song.Playlist.of_url url >>| command
  | [] -> too_few_args
  | _ -> too_many_args
;;

let commands : Command.t list =
  [ { names = [ "start"; "s" ]
    ; summary = "start shuffling songs"
    ; args = ""
    ; parse_args = parse_no_arg start
    }
  ; { names = [ "stop"; "q" ]
    ; summary = "stop playing all songs"
    ; args = ""
    ; parse_args = parse_no_arg stop
    }
  ; { names = [ "skip"; "n" ]
    ; summary = "skip the current song"
    ; args = ""
    ; parse_args = parse_no_arg skip
    }
  ; { names = [ "play"; "p" ]
    ; summary = "queue a song to play next (FIFO)"
    ; args = "<url>"
    ; parse_args = parse_single_video play
    }
  ; { names = [ "play!"; "p!" ]
    ; summary = "play a song immediately, skipping the current"
    ; args = "<url>"
    ; parse_args = parse_single_video play_now
    }
  ; { names = [ "playlist"; "pl" ]
    ; summary = "queue all songs in the play list to play next (FIFO)"
    ; args = "<playlist-url>"
    ; parse_args = parse_single_playlist play_list
    }
  ; { names = [ "ping" ]
    ; summary = "ping yum for a pong"
    ; args = ""
    ; parse_args = parse_no_arg ping
    }
  ; { names = [ "help"; "h" ]
    ; summary = "print this help text"
    ; args = ""
    ; parse_args = parse_no_arg help
    }
  ]
;;

let help_text =
  [ "Available commands:"; "```" ]
  @ List.map commands ~f:Command.help_text
  @ [ "```"; Song.supported_url_formats_msg; "" ]
  @ [ Song.Playlist.supported_url_formats_msg; "" ]
  |> String.concat ~sep:"\n"
;;

let name_to_command =
  List.fold commands ~init:String.Map.empty ~f:(fun acc ({ names; _ } as command) ->
    List.fold names ~init:acc ~f:(fun acc name ->
      match Map.add acc ~key:name ~data:command with
      | `Ok acc -> acc
      | `Duplicate -> Error.raise_s [%message "Duplicate command name" name]))
;;

let parse s =
  match String.split ~on:' ' s |> List.filter ~f:(Fn.compose not String.is_empty) with
  | "yum" :: name :: args ->
    (match Map.find name_to_command name with
     | Some { parse_args; _ } -> parse_args args >>| Option.return
     | None ->
       Or_error.error_string [%string "Command not found: `%{name}`\n%{help_text}"])
  | _ -> Ok None
;;

module%test _ = struct
  let%expect_test "help text" =
    print_endline help_text;
    [%expect
      {|
      Available commands:
      ```
      [ start | s ]                      : start shuffling songs
      [ stop | q ]                       : stop playing all songs
      [ skip | n ]                       : skip the current song
      [ play | p ] <url>                 : queue a song to play next (FIFO)
      [ play! | p! ] <url>               : play a song immediately, skipping the current
      [ playlist | pl ] <playlist-url>   : queue all songs in the play list to play next (FIFO)
      [ ping ]                           : ping yum for a pong
      [ help | h ]                       : print this help text
      ```
      > Supported `<url>` formats:
      > - `[...]https://www.youtube.com/watch?v=<id>[...]`
      > - `[...]https://youtu.be/<id>[...]`
      > - `[...]https://music.youtube.com/watch?v=<id>[...]`
      > - `[...]https://www.bilibili.com/video/<id>[...]`
      > - `[...]https://b23.tv/<id>[...]`

      > Supported `<playlist-url>` formats:
      > - `[...]https://www.youtube.com/playlist?list=<list-id>[...]`
      > - `[...]https://music.youtube.com/playlist?list=<list-id>[...]`
      > - `[...]https://www.youtube.com/watch?v=<video-id>&list=<list-id>[...]`
      > - `[...]https://music.youtube.com/watch?v=<video-id>&list=<list-id>[...]`
      |}]
  ;;
end
