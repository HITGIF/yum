open! Core
open! Async

type t

val create
  :  ffmpeg_path:File_path.Absolute.t
  -> youtube:Youtube.t
  -> guild_id:Discord.Model.Guild_id.t
  -> agent:Agent.t
  -> song_title:Song_title.t
  -> voice_channel:Discord.Model.Channel_id.t
  -> idle_songs:Song.t Nonempty_list.t
  -> frames_writer:Audio.Pcm_frame.t Queue.t Pipe.Writer.t option
  -> on_finished:(unit -> unit Deferred.t)
  -> t

val close : t -> unit
val set_agent : t -> Agent.t -> unit
val voice_channel : t -> Discord.Model.Channel_id.t
val set_voice_channel : t -> Discord.Model.Channel_id.t -> unit
val set_frames_writer : t -> Audio.Pcm_frame.t Queue.t Pipe.Writer.t option -> unit

(* playback *)
val start_once : t -> unit

(** Once the requested songs run out, keep shuffling through the idle songs instead of
    finishing. Without this, the player calls [on_finished] and stops when nothing
    requested is left. *)
val enable_shuffle : t -> unit

val queue : t -> Song.t -> unit
val queue_all : t -> Song.t list -> unit
val play_now : t -> Song.t -> unit
val skip : t -> unit

(* state *)
val started : t -> bool
val playing : t -> Song.t option

(** [None] when nothing is requested and shuffle is off, i.e. the player will finish after
    the current song. *)
val next_song : t -> Song.t option

(** The explicitly-requested songs waiting to play, in order (excludes the
    currently-playing song and the idle-shuffle fallback). *)
val queued : t -> Song.t list
