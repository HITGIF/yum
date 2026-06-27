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
    ; idle_songs : Song.t Nonempty_list.t
    ; ffmpeg_path : File_path.Absolute.t
    ; yt_dlp_path : File_path.Absolute.t
    ; bilibili_sessdata : string option
    ; leave_timer_cancellation_tokens : unit Ivar.t Guild_id.Table.t
    }

  let create ~auth_token ~idle_songs ~ffmpeg_path ~yt_dlp_path ~bilibili_sessdata () =
    { auth_token
    ; players = Guild_id.Table.create ()
    ; idle_songs
    ; ffmpeg_path
    ; yt_dlp_path
    ; bilibili_sessdata
    ; leave_timer_cancellation_tokens = Guild_id.Table.create ()
    }
  ;;

  let player t ~guild_id = Hashtbl.find t.players guild_id

  let cancel_leave_timer t ~guild_id =
    match Hashtbl.find_and_remove t.leave_timer_cancellation_tokens guild_id with
    | Some cancellation_token -> Ivar.fill_if_empty cancellation_token ()
    | None -> ()
  ;;

  let close_player t ~guild_id =
    cancel_leave_timer t ~guild_id;
    player t ~guild_id |> Option.iter ~f:Player.close;
    Hashtbl.remove t.players guild_id
  ;;

  let set_player ?agent t ~guild_id ~voice_channel ~frames_writer =
    Hashtbl.update_and_return t.players guild_id ~f:(function
      | None ->
        let agent =
          Option.value_or_thunk agent ~default:(fun () ->
            Agent.create ~auth_token:t.auth_token ~channel_id:voice_channel)
        in
        let player =
          Player.create
            ~ffmpeg_path:t.ffmpeg_path
            ~yt_dlp_path:t.yt_dlp_path
            ~guild_id
            ~agent
            ~voice_channel
            ~idle_songs:t.idle_songs
            ~frames_writer
        in
        player
      | Some player ->
        Option.iter agent ~f:(Player.set_agent player);
        Player.set_voice_channel player voice_channel;
        Player.set_frames_writer player frames_writer;
        player)
  ;;
end

(* Emoji that tag search results by source. To use a custom server (guild) emoji,
   swap the line to [`Custom (custom "<:name:id>")] — get the "<:name:id>" string
   by typing "\:emojiname:" in Discord (animated emoji render as "<a:name:id>").
   The bot must be a member of a guild that has the emoji. *)
let custom emoji = Agent.Custom_emoji.of_string emoji |> Or_error.ok_exn

let source_emoji (source : Search.Source.t) =
  match source with
  | Youtube -> `Custom (custom "<:Youtubelogo:1520358723359342655>")
  | Bilibili -> `Unicode Agent.Emoji.Regional_indicator_b
;;

let respond ?emoji ?emoji_end agent how_to_respond message =
  match how_to_respond with
  | `Send_message -> Agent.send_message ?emoji ?emoji_end agent message
  | `Respond_interaction (~interaction_id, ~interaction_token) ->
    Agent.respond_interaction
      ?emoji
      ?emoji_end
      agent
      interaction_id
      interaction_token
      message
;;

let join_user_voice ~state ~gateway ~agent ~guild_id ~user_id how_to_respond =
  match%bind Discord.Gateway.join_user_voice gateway ~guild_id ~user_id with
  | `Ok voice_channel ->
    State.set_player state ~agent ~guild_id ~voice_channel ~frames_writer:None
    |> Some
    |> return
  | `User_not_in_voice_channel ->
    let%map () =
      respond ~emoji_end:Pleading_face agent how_to_respond "Join a voice channel first"
    in
    None
;;

let player_or_join_user ~state ~gateway ~agent ~guild_id ~user_id how_to_respond =
  match State.player state ~guild_id with
  | Some player ->
    Player.set_agent player agent;
    Some player |> return
  | None -> join_user_voice ~state ~gateway ~agent ~guild_id ~user_id how_to_respond
;;

let handle_skip ~state ~guild_id ~agent how_to_respond =
  match State.player state ~guild_id with
  | None ->
    let%bind () = respond ~emoji_end:Thinking agent how_to_respond "Not playing" in
    return ()
  | Some player ->
    let%bind () = respond ~emoji_end:Fast_forward agent how_to_respond "Skipped" in
    Player.set_agent player agent;
    Player.skip player;
    return ()
;;

let handle_play ~state ~gateway ~agent ~guild_id ~user_id ~song how_to_respond =
  match%bind
    player_or_join_user ~state ~gateway ~agent ~guild_id ~user_id how_to_respond
  with
  | None -> return ()
  | Some player ->
    if Player.started player
    then (
      Player.queue player song;
      respond ~emoji_end:Arrow_double_up agent how_to_respond "Queued")
    else (
      let%map () = respond ~emoji_end:Arrow_forward agent how_to_respond "Playing" in
      Player.queue player song;
      Player.start_once player |> ignore)
;;

let handle_play_now ~state ~gateway ~agent ~guild_id ~user_id ~song how_to_respond =
  match%bind
    player_or_join_user ~state ~gateway ~agent ~guild_id ~user_id how_to_respond
  with
  | None -> return ()
  | Some player ->
    let%map () = respond ~emoji_end:Arrow_forward agent how_to_respond "Playing" in
    Player.play_now player song;
    Player.start_once player |> ignore
;;

let handle_stop ~state ~gateway ~agent ~guild_id how_to_respond =
  let%bind () = respond ~emoji_end:Stop_button agent how_to_respond "Stopped" in
  State.close_player state ~guild_id;
  Discord.Gateway.leave_voice gateway ~guild_id
;;

let handle_start ~state ~gateway ~agent ~guild_id ~user_id how_to_respond =
  match%bind join_user_voice ~state ~gateway ~agent ~guild_id ~user_id how_to_respond with
  | None -> return ()
  | Some player ->
    let%map () = respond ~emoji_end:Yum agent how_to_respond "Started" in
    Player.start_once player
;;

let handle_search ~(state : State.t) ~agent ~query how_to_respond =
  (* Acknowledge promptly (slash interactions must be answered within 3s, and the
     search itself can be slower), then post the results as their own message. *)
  let%bind () =
    respond ~emoji:Mag agent how_to_respond [%string "Searching for %{query}..."]
  in
  match%bind
    Search.search
      ?bilibili_sessdata:state.bilibili_sessdata
      ~yt_dlp_path:state.yt_dlp_path
      ~query
      ()
  with
  | Error error ->
    let error = [%sexp_of: Error.t] error |> Sexp.to_string_hum in
    Agent.send_message ~code:() ~emoji:Fearful agent error
  | Ok [] ->
    Agent.send_message ~emoji_end:Pleading_face agent [%string "No results for %{query}"]
  | Ok results ->
    let options =
      List.map results ~f:(fun { Search.Result.song; source; label; description } ->
        { Agent.Select.Option.label
        ; (* Discord rejects an empty (but present) description. *)
          description = Option.some_if (not (String.is_empty description)) description
        ; emoji = Some (source_emoji source)
        ; action = Play song
        })
    in
    Agent.send_select
      ~emoji:Clipboard
      ~placeholder:"Pick a song to queue"
      agent
      [%string "Results for %{query}"]
      options
;;

let handle_command ~state ~gateway ~agent ~guild_id ~user_id how_to_respond command =
  match (command : Yum_command.t) with
  | Help -> respond agent how_to_respond Yum_command.Text_command.help_text
  | Ping -> respond ~emoji:Yum agent how_to_respond ""
  | Start -> handle_start ~state ~gateway ~agent ~guild_id ~user_id how_to_respond
  | Stop -> handle_stop ~state ~gateway ~agent ~guild_id how_to_respond
  | Skip -> handle_skip ~state ~guild_id ~agent how_to_respond
  | Play song ->
    handle_play ~state ~gateway ~agent ~guild_id ~user_id ~song how_to_respond
  | Play_now song ->
    handle_play_now ~state ~gateway ~agent ~guild_id ~user_id ~song how_to_respond
  | Search { query } -> handle_search ~state ~agent ~query how_to_respond
  | Play_list playlist ->
    (match Song.Playlist.to_src playlist with
     | `Youtube url ->
       (match%bind Yt_dlp.get_playlist url with
        | Error error ->
          let error = [%sexp_of: Error.t] error |> Sexp.to_string_hum in
          Agent.send_message ~code:() ~emoji:Fearful agent error
        | Ok songs ->
          (match%bind
             player_or_join_user ~state ~gateway ~agent ~guild_id ~user_id how_to_respond
           with
           | None -> return ()
           | Some player ->
             Player.queue_all player songs;
             Player.start_once player |> ignore;
             respond
               ~emoji_end:Arrow_double_up
               agent
               how_to_respond
               [%string "Queued %{List.length songs#Int} songs"])))
;;

let handle_voice_state_update ~state ~gateway ~guild_id =
  match State.player state ~guild_id with
  | None -> ()
  | Some player ->
    let channel_id = Player.voice_channel player in
    let other_users =
      Discord.Gateway.other_users_in_voice_channel gateway ~guild_id ~channel_id
    in
    (match other_users with
     | _ :: _ ->
       if Hashtbl.mem state.leave_timer_cancellation_tokens guild_id
       then (
         [%log.info
           [%here] "Other users joined, cancelling leave timer" (guild_id : Guild_id.t)];
         State.cancel_leave_timer state ~guild_id)
     | [] ->
       if Hashtbl.mem state.leave_timer_cancellation_tokens guild_id
       then ()
       else (
         let leave_after = Time_ns.Span.of_int_sec 30 in
         [%log.info
           [%here]
             "Channel empty, scheduling leave"
             (guild_id : Guild_id.t)
             (leave_after : Time_ns.Span.t)];
         let cancellation_token = Ivar.create () in
         Hashtbl.set
           state.leave_timer_cancellation_tokens
           ~key:guild_id
           ~data:cancellation_token;
         don't_wait_for
           (let%bind result =
              Deferred.any
                [ (let%map () = Clock_ns.after leave_after in
                   `Leave)
                ; (let%map () = Ivar.read cancellation_token in
                   `Cancelled)
                ]
            in
            match result with
            | `Cancelled ->
              [%log.info [%here] "Leave timer cancelled" (guild_id : Guild_id.t)];
              return ()
            | `Leave ->
              [%log.info [%here] "Leaving empty voice channel" (guild_id : Guild_id.t)];
              let agent = Agent.create ~auth_token:state.auth_token ~channel_id in
              let%bind () =
                Agent.send_message ~emoji:Wave agent "Leaving empty voice channel"
              in
              State.close_player state ~guild_id;
              Discord.Gateway.leave_voice gateway ~guild_id)))
;;

let handle_events ~(state : State.t) ~gateway event =
  match (event : Discord.Gateway.Event.t) with
  | Ready { application_id } ->
    Agent.register_slash_commands
      ~auth_token:state.auth_token
      ~application_id
      Yum_command.Slash_command.all
  | Voice_state_update { guild_id } ->
    handle_voice_state_update ~state ~gateway ~guild_id;
    return ()
  | Voice_connected { guild_id = _ } -> return ()
  | Voice { guild_id; event = Voice_ready { channel_id; frames_writer } } ->
    State.set_player
      state
      ~guild_id
      ~voice_channel:channel_id
      ~frames_writer:(Some frames_writer)
    |> ignore;
    return ()
  | Interaction
      { id = interaction_id
      ; token = interaction_token
      ; guild_id
      ; channel_id
      ; user = { id = user_id; _ }
      ; custom_id
      ; component_type = _
      ; values
      } ->
    let agent = Agent.create ~auth_token:state.auth_token ~channel_id in
    let how_to_respond = `Respond_interaction (~interaction_id, ~interaction_token) in
    (* A button click carries its action in [custom_id]; a select pick carries it
       as the chosen option's value in [values]. *)
    let action_id =
      match values with
      | value :: _ -> value
      | [] -> custom_id
    in
    (match Agent.Action.of_custom_id action_id with
     | Skip -> handle_skip ~state ~guild_id ~agent how_to_respond
     | Stop -> handle_stop ~state ~gateway ~agent ~guild_id how_to_respond
     | Start -> handle_start ~state ~gateway ~agent ~guild_id ~user_id how_to_respond
     | Play song ->
       handle_play ~state ~gateway ~agent ~guild_id ~user_id ~song how_to_respond
     | Play_now song ->
       handle_play_now ~state ~gateway ~agent ~guild_id ~user_id ~song how_to_respond
     | Unknown _ -> return ())
  | Slash_command
      { id = interaction_id
      ; token = interaction_token
      ; guild_id
      ; channel_id
      ; user = { id = user_id; _ }
      ; name
      ; options
      } ->
    let agent = Agent.create ~auth_token:state.auth_token ~channel_id in
    let how_to_respond = `Respond_interaction (~interaction_id, ~interaction_token) in
    (match Yum_command.Slash_command.parse ~name ~options with
     | Ok command ->
       handle_command ~state ~gateway ~agent ~guild_id ~user_id how_to_respond command
     | Error error ->
       respond ~emoji:Pleading_face agent how_to_respond (Error.to_string_hum error))
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
       let agent =
         Agent.create ~auth_token:state.auth_token ~channel_id:message_channel
       in
       (match Yum_command.Text_command.parse content with
        | Ok None -> return ()
        | Ok (Some command) ->
          handle_command ~state ~gateway ~agent ~guild_id ~user_id `Send_message command
        | Error error ->
          let error = Error.to_string_hum error in
          Agent.send_message ~emoji:Pleading_face agent error))
;;

let read_youtube_songs filename =
  In_channel.read_lines filename
  |> List.permute
  |> Nonempty_list.of_list
  |> Option.value_exn ~message:"Empty Youtube songs file."
  |> Nonempty_list.map ~f:Song.of_youtube_string
;;

let run
  ~discord_bot_token:auth_token
  ~youtube_songs
  ~ffmpeg_path
  ~yt_dlp_path
  ~bilibili_sessdata
  ()
  =
  Gc.disable_compaction ~allocation_policy:`Don't_change ();
  Scheduler.report_long_cycle_times ~cutoff:(Time_float.Span.of_int_ms 100) ();
  let youtube_songs = read_youtube_songs youtube_songs in
  let state =
    State.create
      ~auth_token
      ~idle_songs:youtube_songs
      ~ffmpeg_path
      ~yt_dlp_path
      ~bilibili_sessdata
      ()
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
