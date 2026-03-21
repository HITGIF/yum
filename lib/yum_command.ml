open! Core
open Or_error.Let_syntax

type t =
  | Start
  | Stop
  | Skip
  | Play of Song.t
  | Play_now of Song.t
  | Play_list of Song.Playlist.t
  | Ping
  | Help
[@@deriving variants]

module Slash_command_option =
  Discord.Model.Gateway.Event.Dispatch.Interaction_create.Slash_command_option

module Entry = struct
  type nonrec t =
    { description : string
    ; text_names : string list
    ; text_args : string
    ; parse_text_args : string list -> t Or_error.t
    ; slash_name : string
    ; slash_options : Discord.Model.Slash_command.Option.t list
    ; parse_slash_options : Slash_command_option.t list -> t Or_error.t
    }
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

let find_url options =
  match
    List.find_map options ~f:(fun { Slash_command_option.name; value } ->
      if String.equal name "url" then Some value else None)
  with
  | Some url -> Ok url
  | None -> Or_error.error_string "Missing required option: url"
;;

let url_option ~description : Discord.Model.Slash_command.Option.t =
  { type_ = String; name = "url"; description; required = true }
;;

let no_options ~slash_name ~text_names ~description ~command : Entry.t =
  { description
  ; text_names
  ; text_args = ""
  ; parse_text_args = parse_no_arg command
  ; slash_name
  ; slash_options = []
  ; parse_slash_options = (fun _ -> Ok command)
  }
;;

let with_url
  ~slash_name
  ~text_names
  ~description
  ~arg_text_description
  ~arg_slash_description
  ~parse
  : Entry.t
  =
  { description
  ; text_names
  ; text_args = arg_text_description
  ; parse_text_args = parse
  ; slash_name
  ; slash_options = [ url_option ~description:arg_slash_description ]
  ; parse_slash_options =
      (fun options ->
        let%bind.Or_error url = find_url options in
        parse [ url ])
  }
;;

let entries : Entry.t list =
  let f entry acc _ = entry :: acc in
  List.rev
    (Variants.fold
       ~init:[]
       ~ping:
         (no_options
            ~slash_name:"ping"
            ~text_names:[ "ping" ]
            ~description:"Ping yum for a pong"
            ~command:Ping
          |> f)
       ~start:
         (no_options
            ~slash_name:"start"
            ~text_names:[ "start"; "s" ]
            ~description:"Start shuffling songs"
            ~command:Start
          |> f)
       ~stop:
         (no_options
            ~slash_name:"stop"
            ~text_names:[ "stop"; "q" ]
            ~description:"Stop playing all songs"
            ~command:Stop
          |> f)
       ~skip:
         (no_options
            ~slash_name:"skip"
            ~text_names:[ "skip"; "n" ]
            ~description:"Skip the current song"
            ~command:Skip
          |> f)
       ~help:
         (no_options
            ~slash_name:"help"
            ~text_names:[ "help"; "h" ]
            ~description:"Print help text"
            ~command:Help
          |> f)
       ~play:
         (with_url
            ~slash_name:"play"
            ~text_names:[ "play"; "p" ]
            ~description:"Queue a song to play next"
            ~arg_text_description:"<video-url>"
            ~arg_slash_description:"Video URL"
            ~parse:(parse_single_video play)
          |> f)
       ~play_now:
         (with_url
            ~slash_name:"play-now"
            ~text_names:[ "play!"; "p!" ]
            ~description:"Play a song immediately, skipping the current"
            ~arg_text_description:"<video-url>"
            ~arg_slash_description:"Video URL"
            ~parse:(parse_single_video play_now)
          |> f)
       ~play_list:
         (with_url
            ~slash_name:"playlist"
            ~text_names:[ "playlist"; "pl" ]
            ~description:"Queue all songs in a playlist"
            ~arg_text_description:"<playlist-url>"
            ~arg_slash_description:"Playlist URL"
            ~parse:(parse_single_playlist play_list)
          |> f))
;;

module Text_command = struct
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

  let all =
    List.map entries ~f:(fun { description; text_names; text_args; parse_text_args; _ } ->
      { names = text_names
      ; summary = String.uncapitalize description
      ; args = text_args
      ; parse_args = parse_text_args
      })
  ;;

  let help_text =
    [ "Available commands:"; "```" ]
    @ List.map all ~f:help_text
    @ [ "```"; Song.supported_url_formats_msg; "" ]
    @ [ Song.Playlist.supported_url_formats_msg; "" ]
    |> String.concat ~sep:"\n"
  ;;

  let name_to_command =
    List.fold all ~init:String.Map.empty ~f:(fun acc ({ names; _ } as command) ->
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
end

module Slash_command = struct
  let all =
    List.map entries ~f:(fun { slash_name; description; slash_options; _ } ->
      { Discord.Model.Slash_command.name = slash_name
      ; type_ = Chat_input
      ; description
      ; options = slash_options
      })
  ;;

  let name_to_entry =
    List.map entries ~f:(fun ({ slash_name; _ } as entry) -> slash_name, entry)
    |> String.Map.of_alist_exn
  ;;

  let parse ~name ~options =
    match Map.find name_to_entry name with
    | Some { parse_slash_options; _ } -> parse_slash_options options
    | None -> Or_error.error_string [%string "Unknown slash command: %{name}"]
  ;;
end

module%test _ = struct
  let%expect_test "help text" =
    print_endline Text_command.help_text;
    [%expect
      {|
      Available commands:
      ```
      [ start | s ]                      : start shuffling songs
      [ stop | q ]                       : stop playing all songs
      [ skip | n ]                       : skip the current song
      [ play | p ] <video-url>           : queue a song to play next
      [ play! | p! ] <video-url>         : play a song immediately, skipping the current
      [ playlist | pl ] <playlist-url>   : queue all songs in a playlist
      [ ping ]                           : ping yum for a pong
      [ help | h ]                       : print help text
      ```
      > Supported `<video-url>` formats:
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
