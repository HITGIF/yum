open! Core
open! Async
open! Common

open struct
  open Discord.Model
  module Auth_token = Auth_token
  module Channel_id = Channel_id
  module Guild_id = Guild_id
  module Message = Message
  module Message_id = Message_id
  module Uri = Uri
end

module State = struct
  type t =
    { auth_token : Auth_token.t
    ; players : Player.t Guild_id.Table.t
    ; default_songs : Song.t Nonempty_list.t
    ; ffmpeg_path : File_path.Absolute.t
    ; youtube_dl_path : File_path.Absolute.t
    }

  let create ~auth_token ~default_songs ~ffmpeg_path ~youtube_dl_path () =
    { auth_token
    ; players = Guild_id.Table.create ()
    ; default_songs
    ; ffmpeg_path
    ; youtube_dl_path
    }
  ;;

  let player t ~guild_id = Hashtbl.find t.players guild_id

  let close_player t ~guild_id =
    player t ~guild_id |> Option.iter ~f:Player.close;
    Hashtbl.remove t.players guild_id
  ;;

  let set_player ?message_channel t ~guild_id ~voice_channel ~frames_writer =
    Hashtbl.update_and_return t.players guild_id ~f:(function
      | None ->
        let message_channel = Option.value message_channel ~default:voice_channel in
        let player =
          Player.create
            ~auth_token:t.auth_token
            ~ffmpeg_path:t.ffmpeg_path
            ~youtube_dl_path:t.youtube_dl_path
            ~guild_id
            ~voice_channel
            ~message_channel
            ~default_songs:t.default_songs
            ~frames_writer
        in
        player
      | Some player ->
        Option.iter message_channel ~f:(Player.set_message_channel player);
        Player.set_voice_channel player voice_channel;
        Player.set_frames_writer player frames_writer;
        player)
  ;;

  let send_message t channel_id content =
    Discord.Http.Create_message.call
      ~auth_token:t.auth_token
      ~channel_id
      { content = Some (Dedent.string content) }
    |> Deferred.ignore_m
  ;;
end

let join_user_voice ~state ~gateway ~guild_id ~message_channel ~user_id =
  match%bind Discord.Gateway.join_user_voice gateway ~guild_id ~user_id with
  | `Ok voice_channel ->
    State.set_player state ~guild_id ~voice_channel ~message_channel ~frames_writer:None
    |> Some
    |> return
  | `User_not_in_voice_channel ->
    let%bind () =
      State.send_message state message_channel "Join a voice channel first :thinking:"
    in
    return None
;;

let player_or_join_user ~state ~gateway ~guild_id ~message_channel ~user_id =
  match State.player state ~guild_id with
  | Some player ->
    Player.set_message_channel player message_channel;
    Some player |> return
  | None -> join_user_voice ~state ~gateway ~guild_id ~message_channel ~user_id
;;

let handle_command ~state ~gateway ~guild_id ~message_channel ~user_id command =
  let send_message = State.send_message state in
  match (command : Yum_command.t) with
  | Help -> send_message message_channel Yum_command.help_text
  | Ping -> send_message message_channel ":yum:"
  | Start ->
    (match%bind join_user_voice ~state ~gateway ~guild_id ~message_channel ~user_id with
     | None -> return ()
     | Some player ->
       (match Player.start_once player with
        | `Ok -> return ()
        | `Already_started -> send_message message_channel "Rejoined :yum:"))
  | Stop ->
    State.close_player state ~guild_id;
    Discord.Gateway.leave_voice gateway ~guild_id
  | Skip ->
    (match State.player state ~guild_id with
     | None -> return ()
     | Some player ->
       Player.set_message_channel player message_channel;
       Player.skip player;
       return ())
  | Play song ->
    (match%bind
       player_or_join_user ~state ~gateway ~guild_id ~message_channel ~user_id
     with
     | None -> return ()
     | Some player ->
       Player.queue player song;
       (match Player.start_once player with
        | `Ok -> return ()
        | `Already_started -> send_message message_channel "Queued :yum:"))
  | Play_now song ->
    (match%bind
       player_or_join_user ~state ~gateway ~guild_id ~message_channel ~user_id
     with
     | None -> return ()
     | Some player ->
       Player.play_now player song;
       Player.start_once player |> ignore;
       return ())
  | Play_list playlist ->
    (match Song.Playlist.to_src playlist with
     | `Ytdl_playlist url ->
       (match%bind Youtube_dl.get_playlist url with
        | Error error ->
          let error = [%sexp_of: Error.t] error |> Sexp.to_string_hum in
          send_message message_channel [%string ":fearful: ```%{error}```"]
        | Ok songs ->
          (match%bind
             player_or_join_user ~state ~gateway ~guild_id ~message_channel ~user_id
           with
           | None -> return ()
           | Some player ->
             Player.queue_all player songs;
             Player.start_once player |> ignore;
             send_message
               message_channel
               [%string "Queued %{List.length songs#Int} songs :yum:"])))
;;

let handle_events ~state ~gateway event =
  match (event : Discord.Gateway.Event.t) with
  | Voice_connected { guild_id = _ } -> return ()
  | Voice { guild_id; event = Voice_ready { channel_id; frames_writer } } ->
    State.set_player
      state
      ~guild_id
      ~voice_channel:channel_id
      ~frames_writer:(Some frames_writer)
    |> ignore;
    return ()
  | Message
      { id = message_id
      ; guild_id
      ; channel_id = message_channel
      ; author = { id = user_id; _ }
      ; content
      ; timestamp = _
      ; edited_timestamp = _
      ; type_ = _
      } ->
    (match guild_id with
     | None ->
       [%log.debug
         [%here] "Ignoring message without guild ID" (message_id : Message_id.t)];
       return ()
     | Some guild_id ->
       (match Yum_command.parse content with
        | Ok None -> return ()
        | Ok (Some command) ->
          handle_command ~state ~gateway ~guild_id ~message_channel ~user_id command
        | Error error ->
          let error = Error.to_string_hum error in
          State.send_message state message_channel [%string ":pleading_face: %{error}"]))
;;

let read_youtube_songs filename =
  In_channel.read_lines filename
  |> List.permute
  |> Nonempty_list.of_list
  |> Option.value_exn ~message:"Empty Youtube songs file."
  |> Nonempty_list.map ~f:Song.of_youtube_string
;;

let run ~discord_bot_token:auth_token ~youtube_songs ~ffmpeg_path ~youtube_dl_path () =
  Gc.disable_compaction ~allocation_policy:`Don't_change ();
  Scheduler.report_long_cycle_times ~cutoff:(Time_float.Span.of_int_ms 100) ();
  let youtube_songs = read_youtube_songs youtube_songs in
  let state =
    State.create ~auth_token ~default_songs:youtube_songs ~ffmpeg_path ~youtube_dl_path ()
  in
  let%with (`Shutdown shutdown) = Graceful_shutdown.with_ in
  let%with gateway =
    Discord.Gateway.with_
      ~initial_gateway_url:(Uri.of_string "wss://gateway.discord.gg")
      ~auth_token
      ~intents:[ Guilds; Guild_voice_states; Guild_messages; Message_content ]
      ~properties:{ os = "linux"; browser = "yum"; device = "yum" }
  in
  Discord.Gateway.events gateway
  |> Pipe.iter ~f:(handle_events ~state ~gateway)
  |> don't_wait_for;
  Deferred.ok shutdown
;;
