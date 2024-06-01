open! Core

module Video_id : sig
  type t

  val of_string : string -> t
  val to_url : t -> string
  val of_url_exn : string -> t
end = struct
  type t = string

  let prefix = "https://www.youtube.com/watch?v="
  let of_string = Fn.id
  let to_url t = [%string "%{prefix}%{t}"]
  let of_url_exn t = String.chop_prefix_exn ~prefix t
end

module Guild_state = struct
  module State = struct
    type t =
      { channel_id : string
      ; queued_videos : Video_id.t Deque.t
      }
  end

  type t =
    | Idle
    | Joining of State.t
    | Playing of State.t

  let init = Idle
end

module State = struct
  type t = Guild_state.t String.Map.t

  let init = String.Map.empty
end

module Command = struct
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
       | [ "play!"; url ] | [ "p!"; url ] ->
         Ok (Some (Play_now (Video_id.of_url_exn url)))
       | _ -> Or_error.error_string "Invalid command")
    | _ -> Ok None
  ;;
end

let read_videos ~videos_file_path =
  In_channel.read_lines videos_file_path
  |> List.map ~f:Video_id.of_string
  |> List.permute
  |> List.to_array
  |> Deque.of_array
;;

let send_message rest message ~channel_id =
  if Discord.Rest.make_create_message_param ~content:message ()
     |> Discord.Rest.create_message channel_id rest
     |> Result.is_error
  then Logs.err (fun m -> m "Failed to send message")
;;

let join agent ~guild_id ~user_id =
  match agent |> Discord.Agent.get_voice_states ~guild_id ~user_id with
  | None -> ()
  | Some vstate ->
    (match vstate.Discord.Event.channel_id with
     | None -> ()
     | Some channel_id -> agent |> Discord.Agent.join_channel ~guild_id ~channel_id)
;;

let leave ~guild_id = Discord.Agent.leave_channel ~guild_id

let play agent rest ~guild_id ~channel_id ~url =
  Logs.info (fun m -> m "Playing %s" url);
  send_message rest ~channel_id [%string "Playing %{url}"];
  Discord.Agent.play_voice agent ~guild_id ~src:(`Ytdl url)
;;

let handle_event _env ~sw:_ ~videos_file_path agent rest (state : State.t) event =
  match event with
  | Discord.Event.Dispatch (MESSAGE_CREATE msg) ->
    let guild_id = Option.value_exn msg.guild_id in
    let guild_state = Map.find state guild_id |> Option.value ~default:Guild_state.init in
    let channel_id = msg.channel_id in
    let guild_state =
      match Command.parse msg.content with
      | Error e ->
        let e = Error.to_string_hum e in
        send_message rest ~channel_id [%string "Error: %{e}"];
        guild_state
      | Ok None -> guild_state
      | Ok (Some command) ->
        (match command with
         | Ping ->
           send_message rest "pong" ~channel_id;
           guild_state
         | Start ->
           (match guild_state with
            | Playing _ | Joining _ ->
              send_message rest ~channel_id "Already started";
              guild_state
            | Idle ->
              let queued_videos = read_videos ~videos_file_path in
              join agent ~guild_id ~user_id:msg.author.id;
              Joining { channel_id; queued_videos })
         | Stop ->
           send_message rest ~channel_id "Stopped";
           leave agent ~guild_id;
           Guild_state.Idle
         | Skip ->
           (match guild_state with
            | Idle | Joining _ -> guild_state
            | Playing { channel_id; queued_videos } ->
              Discord.Agent.skip agent ~guild_id;
              send_message rest ~channel_id "Skipped";
              (match Deque.dequeue_front queued_videos with
               | Some video_id ->
                 play agent rest ~guild_id ~channel_id ~url:(Video_id.to_url video_id);
                 guild_state
               | None ->
                 send_message rest ~channel_id "Done";
                 leave agent ~guild_id;
                 Idle))
         | Play video_id ->
           (match guild_state with
            | Playing { queued_videos; _ } | Joining { queued_videos; _ } ->
              Deque.enqueue_front queued_videos video_id;
              send_message rest ~channel_id "Queued for next";
              guild_state
            | Idle ->
              join agent ~guild_id ~user_id:msg.author.id;
              Joining { channel_id; queued_videos = Deque.of_array [| video_id |] })
         | Play_now video_id ->
           (match guild_state with
            | Playing _ | Joining _ ->
              Discord.Agent.skip agent ~guild_id;
              play agent rest ~guild_id ~channel_id ~url:(Video_id.to_url video_id);
              guild_state
            | Idle ->
              join agent ~guild_id ~user_id:msg.author.id;
              Joining { channel_id; queued_videos = Deque.of_array [| video_id |] }))
    in
    Map.set state ~key:guild_id ~data:guild_state
  | VoiceReady { guild_id } ->
    let guild_state = Map.find state guild_id |> Option.value ~default:Guild_state.init in
    let guild_state =
      match guild_state with
      | Idle | Playing _ -> guild_state
      | Joining { channel_id; queued_videos } ->
        (match Deque.dequeue_front queued_videos with
         | Some video_id ->
           play agent rest ~guild_id ~channel_id ~url:(Video_id.to_url video_id);
           Playing { channel_id; queued_videos }
         | None -> Idle)
    in
    Map.set state ~key:guild_id ~data:guild_state
  | VoiceSpeaking { guild_id; speaking } ->
    let guild_state = Map.find state guild_id |> Option.value ~default:Guild_state.init in
    let guild_state =
      if speaking
      then guild_state
      else (
        match guild_state with
        | Idle | Joining _ -> guild_state
        | Playing { channel_id; queued_videos } ->
          (match Deque.dequeue_front queued_videos with
           | Some video_id ->
             play agent rest ~guild_id ~channel_id ~url:(Video_id.to_url video_id);
             guild_state
           | None ->
             send_message rest ~channel_id "Done";
             leave agent ~guild_id;
             Idle))
    in
    Map.set state ~key:guild_id ~data:guild_state
  | _ -> state
;;

let main ~discord_token ~videos_file_path ~youtubedl_path ~ffmpeg_path =
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env
  @@ fun () ->
  Eio.Switch.run
  @@ fun sw ->
  let _consumer : _ Discord.Consumer.t =
    Discord.Consumer.start
      env
      ~sw
      ~token:discord_token
      ~intents:
        (Discord.Intent.encode
           [ GUILDS; GUILD_VOICE_STATES; GUILD_MESSAGES; MESSAGE_CONTENT ])
      ?ffmpeg_path
      ?youtubedl_path
      (fun () -> State.init)
      (handle_event ~videos_file_path)
  in
  ()
;;

let command =
  let open Core.Command.Param in
  let optional_param ~arg ~arg_type ~env ~doc =
    flag arg (optional arg_type) ~doc:(doc ^ " (env: " ^ env ^ ")")
    |> map ~f:(Option.value_map ~f:Option.return ~default:(Sys.getenv env))
  in
  let required_param ~arg ~arg_type ~env ~doc ~if_missing =
    optional_param ~arg ~arg_type ~env ~doc
    |> map ~f:(fun param ->
      match if_missing with
      | `Default default -> Option.value param ~default
      | `Raise ->
        Option.value_exn
          param
          ~error:
            (Error.create_s
               [%message "Missing required parameter" ~flag:(arg : string) (env : string)]))
  in
  Core.Command.basic
    ~summary:"yum"
    (let%map_open.Core.Command discord_token =
       required_param
         ~arg:"-discord-bot-token"
         ~arg_type:string
         ~env:"YUM_DISCORD_BOT_TOKEN"
         ~doc:"STRING Discord bot token"
         ~if_missing:`Raise
     and videos_file_path =
       required_param
         ~arg:"-videos-file"
         ~arg_type:Filename_unix.arg_type
         ~env:"YUM_VIDEOS_FILE"
         ~doc:"FILE Path to the file containing the list of YouTube video ids"
         ~if_missing:(`Default "videos.txt")
     and youtubedl_path =
       optional_param
         ~arg:"-youtubedl-path"
         ~arg_type:Filename_unix.arg_type
         ~env:"YUM_YOUTUBEDL_PATH"
         ~doc:"FILE Path to the youtube-dl binary"
     and ffmpeg_path =
       optional_param
         ~arg:"-ffmpeg-path"
         ~arg_type:Filename_unix.arg_type
         ~env:"YUM_FFMPEG_PATH"
         ~doc:"FILE Path to the ffmpeg binary"
     in
     fun () -> main ~discord_token ~videos_file_path ~youtubedl_path ~ffmpeg_path)
;;

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Command_unix.run command
;;
