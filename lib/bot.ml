open! Core
open Types

let read_videos ~videos_file_path =
  In_channel.read_lines videos_file_path
  |> List.map ~f:Video_id.of_youtube_string
  |> List.permute
  |> List.to_array
  |> Deque.of_array
;;

let send_message rest message ~channel_id =
  match
    Discord.Rest.make_create_message_param ~content:message ()
    |> Discord.Rest.create_message channel_id rest
  with
  | Ok _ -> ()
  | Error err -> Logs.err (fun m -> m "Failed to send message: %s" err)
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

let play agent rest ~guild_id ~channel_id ~video_id =
  let url = Video_id.to_url video_id in
  Logs.info (fun m -> m "Playing %s" url);
  send_message rest ~channel_id [%string "Playing %{url}"];
  Discord.Agent.play_voice agent ~guild_id ~src:(`Ytdl url)
;;

let handle_event _env ~sw:_ ~videos_file_path agent rest state = function
  | Discord.Event.Dispatch (MESSAGE_CREATE msg) ->
    let guild_id = Option.value_exn msg.guild_id in
    let guild_state =
      Map.find state guild_id |> Option.value ~default:State.Guild_state.init
    in
    let channel_id = msg.channel_id in
    let guild_state =
      match Yum_command.parse msg.content with
      | Error e ->
        let e = Error.to_string_hum e in
        send_message rest ~channel_id [%string "Error: %{e}"];
        guild_state
      | Ok None -> guild_state
      | Ok (Some command) ->
        (match command with
         | Help ->
           send_message rest Yum_command.help_text ~channel_id;
           guild_state
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
           State.Guild_state.Idle
         | Skip ->
           (match guild_state with
            | Idle | Joining _ -> guild_state
            | Playing { channel_id; queued_videos } ->
              Discord.Agent.skip agent ~guild_id;
              send_message rest ~channel_id "Skipped";
              (match Deque.dequeue_front queued_videos with
               | Some video_id ->
                 play agent rest ~guild_id ~channel_id ~video_id;
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
              play agent rest ~guild_id ~channel_id ~video_id;
              guild_state
            | Idle ->
              join agent ~guild_id ~user_id:msg.author.id;
              Joining { channel_id; queued_videos = Deque.of_array [| video_id |] }))
    in
    Map.set state ~key:guild_id ~data:guild_state
  | VoiceReady { guild_id } ->
    let guild_state =
      Map.find state guild_id |> Option.value ~default:State.Guild_state.init
    in
    let guild_state =
      match guild_state with
      | Idle | Playing _ -> guild_state
      | Joining { channel_id; queued_videos } ->
        (match Deque.dequeue_front queued_videos with
         | Some video_id ->
           play agent rest ~guild_id ~channel_id ~video_id;
           Playing { channel_id; queued_videos }
         | None -> Idle)
    in
    Map.set state ~key:guild_id ~data:guild_state
  | VoiceSpeaking { guild_id; speaking } ->
    let guild_state =
      Map.find state guild_id |> Option.value ~default:State.Guild_state.init
    in
    let guild_state =
      if speaking
      then guild_state
      else (
        match guild_state with
        | Idle | Joining _ -> guild_state
        | Playing { channel_id; queued_videos } ->
          (match Deque.dequeue_front queued_videos with
           | Some video_id ->
             play agent rest ~guild_id ~channel_id ~video_id;
             guild_state
           | None ->
             send_message rest ~channel_id "Done :yum:";
             leave agent ~guild_id;
             Idle))
    in
    Map.set state ~key:guild_id ~data:guild_state
  | _ -> state
;;
