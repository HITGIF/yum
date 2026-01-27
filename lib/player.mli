open! Core
open! Async

type t

val create
  :  ffmpeg_path:File_path.Absolute.t
  -> yt_dlp_path:File_path.Absolute.t
  -> guild_id:Discord.Model.Guild_id.t
  -> agent:Agent.t
  -> voice_channel:Discord.Model.Channel_id.t
  -> default_songs:Song.t Nonempty_list.t
  -> frames_writer:Audio.Pcm_frame.t Queue.t Pipe.Writer.t option
  -> t

val close : t -> unit
val set_agent : t -> Agent.t -> unit
val set_voice_channel : t -> Discord.Model.Channel_id.t -> unit
val set_frames_writer : t -> Audio.Pcm_frame.t Queue.t Pipe.Writer.t option -> unit

(* playback *)
val start_once : t -> unit
val queue : t -> Song.t -> unit
val queue_all : t -> Song.t list -> unit
val play_now : t -> Song.t -> unit
val skip : t -> unit

(* state *)
val started : t -> bool
val playing : t -> Song.t option
val next_song : t -> Song.t
