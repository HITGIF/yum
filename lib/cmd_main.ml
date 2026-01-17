open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"😋 A Discord music player bot."
    (let%map_open.Command () = Log.set_level_via_param (force Log.Global.log)
     and discord_bot_token =
       flag
         [%var_dash_name]
         ~doc:"STRING Discord bot auth token"
         (required Discord.Model.Auth_token.arg_type)
     and youtube_songs =
       flag [%var_dash_name] ~doc:"FILE Youtube songs file" (required string)
     and ffmpeg_path =
       flag_optional_with_default_doc_string
         [%var_dash_name]
         File_path.Absolute.arg_type
         File_path.Absolute.to_string
         ~default:Ffmpeg.default_prog
         ~doc:"PATH Path to the ffmpeg binary"
     and youtube_dl_path =
       flag_optional_with_default_doc_string
         [%var_dash_name]
         File_path.Absolute.arg_type
         File_path.Absolute.to_string
         ~default:Youtube_dl.default_prog
         ~doc:"PATH Path to the youtube-dl binary"
     and media_get_path =
       flag_optional_with_default_doc_string
         [%var_dash_name]
         File_path.Absolute.arg_type
         File_path.Absolute.to_string
         ~default:Media_get.default_prog
         ~doc:"PATH Path to the media-get binary"
     in
     fun () ->
       Server.run
         ~discord_bot_token
         ~youtube_songs
         ~ffmpeg_path
         ~youtube_dl_path
         ~media_get_path
         ())
;;
