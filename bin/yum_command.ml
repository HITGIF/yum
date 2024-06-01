open! Core

type t =
  | Ping
  | Start
  | Skip
  | Stop
  | Play of Video_id.t
  | Play_now of Video_id.t

let parse s =
  match String.split ~on:' ' s |> List.filter ~f:(Fn.compose not String.is_empty) with
  | "yum" :: args ->
    (match args with
     | [ "ping" ] -> Ok (Some Ping)
     | [ "start" ] -> Ok (Some Start)
     | [ "skip" ] | [ "next" ] | [ "n" ] | [ "sk" ] -> Ok (Some Skip)
     | [ "stop" ] -> Ok (Some Stop)
     | [ "play"; url ] | [ "p"; url ] -> Ok (Some (Play (Video_id.of_url_exn url)))
     | [ "play!"; url ] | [ "p!"; url ] -> Ok (Some (Play_now (Video_id.of_url_exn url)))
     | _ -> Or_error.error_string "Invalid command")
  | _ -> Ok None
;;
