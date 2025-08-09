open! Core
open Yum

let default_ffmpeg_path = "/usr/bin/ffmpeg"

let default_ffmpeg_options =
  [ "-i"
  ; "pipe:0"
  ; "-ac"
  ; "2"
  ; "-ar"
  ; "48000"
  ; "-f"
  ; "s16le"
  ; "-loglevel"
  ; "quiet"
  ; "pipe:1"
  ]
;;

let default_youtubedl_path = "/usr/bin/youtube-dl"
let default_media_get_path = "/usr/bin/media-get"

let main
      ?(ffmpeg_path = default_ffmpeg_path)
      ?(ffmpeg_options = default_ffmpeg_options)
      ?(youtubedl_path = default_youtubedl_path)
      ?(media_get_path = default_media_get_path)
      ~discord_token
      ~videos_file_path
      ()
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run
  @@ fun env ->
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
      ~ffmpeg_path
      ~ffmpeg_options
      ~youtubedl_path
      ~media_get_path
      (fun () -> State.init)
      (Bot.handle_event ~videos_file_path ~youtubedl_path)
  in
  ()
;;

let main_command =
  let open Command.Param in
  let optional_param ~arg ~arg_type ~env ~doc =
    flag arg (optional arg_type) ~doc:(doc ^ [%string " (env: %{env})"])
    |> map ~f:(Option.value_map ~f:Option.return ~default:(Sys.getenv env))
  in
  let required_param ~arg ~arg_type ~env ~doc ~if_missing =
    optional_param
      ~arg
      ~arg_type
      ~env
      ~doc:
        (match if_missing with
         | `Raise -> doc
         | `Default default -> doc ^ [%string " (default: %{default})"])
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
  Command.basic
    ~summary:"😋 A Discord music player bot, based on discordml."
    (let%map_open.Command discord_token =
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
         ~doc:
           [%string
             "FILE Path to the youtube-dl binary (default: %{default_youtubedl_path})"]
     and ffmpeg_path =
       optional_param
         ~arg:"-ffmpeg-path"
         ~arg_type:Filename_unix.arg_type
         ~env:"YUM_FFMPEG_PATH"
         ~doc:[%string "FILE Path to the ffmpeg binary (default: %{default_ffmpeg_path})"]
     and media_get_path =
       optional_param
         ~arg:"-media-get-path"
         ~arg_type:Filename_unix.arg_type
         ~env:"YUM_MEDIA_GET_PATH"
         ~doc:
           [%string
             "FILE Path to the media-get binary (default: %{default_media_get_path})"]
     in
     fun () ->
       main
         ?youtubedl_path
         ?ffmpeg_path
         ?media_get_path
         ~discord_token
         ~videos_file_path
         ())
;;

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Command_unix.run main_command
;;
