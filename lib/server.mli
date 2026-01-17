open! Core
open! Async

val run
  :  discord_bot_token:Discord.Model.Auth_token.t
  -> youtube_songs:Filename.t
  -> ffmpeg_path:File_path.Absolute.t
  -> youtube_dl_path:File_path.Absolute.t
  -> media_get_path:File_path.Absolute.t
  -> unit
  -> unit Deferred.Or_error.t
