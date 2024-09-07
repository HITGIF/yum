open! Core

type t =
  | Ping
  | Start
  | Stop
  | Skip
  | Help
  | Play of Video_id.t
  | Play_now of Video_id.t
[@@deriving variants]

module Command = struct
  type nonrec t =
    { names : string list
    ; summary : string
    ; args : string
    ; parse_args : string list -> t Or_error.t
    }

  let command_synopsis_length = 25
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
  | [ url ] -> Ok (command (Video_id.of_url_exn url))
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
    ; summary = "queue a song to play next (LIFO)"
    ; args = "<url>"
    ; parse_args = parse_single_video play
    }
  ; { names = [ "play!"; "p!" ]
    ; summary = "play a song immediately"
    ; args = "<url>"
    ; parse_args = parse_single_video play_now
    }
  ; { names = [ "ping" ]
    ; summary = "ping yum for a pong"
    ; args = ""
    ; parse_args = parse_no_arg ping
    }
  ]
;;

let help_text =
  [ "Available commands:"; "```" ]
  @ List.map commands ~f:Command.help_text
  @ [ Command.pad_synopsis "[ help | h ]" ^ ": print this help text"
    ; "```"
    ; "> Supported `<url>` format:"
    ; "> - `https://www.youtube.com/watch?v=[...]`"
    ; ""
    ]
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
  | "yum" :: "help" :: _ | "yum" :: "h" :: _ -> Ok (Some Help)
  | "yum" :: name :: args ->
    (match Map.find name_to_command name with
     | None ->
       Or_error.error_string [%string "Command not found: `%{name}`\n%{help_text}"]
     | Some { parse_args; _ } ->
       let%map.Or_error command = parse_args args in
       Some command)
  | _ -> Ok None
;;
