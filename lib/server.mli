open! Core
open! Async

val run
  :  discord_bot_token:Discord.Model.Auth_token.t
  -> youtube_songs:Filename.t
  -> ffmpeg_path:File_path.Absolute.t
  -> youtube:Youtube.t
  -> bilibili_sessdata:string option
  -> unit
  -> unit Deferred.Or_error.t
