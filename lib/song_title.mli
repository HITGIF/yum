open! Core
open! Async

(** Lazily fetches and caches each song's human-readable title. *)
type t

val create
  :  yt_dlp_path:File_path.Absolute.t
  -> bilibili_sessdata:string option
  -> t

(** [get t song] returns [song]'s title, fetching it on first request (yt-dlp for
    YouTube, the bilibili view API for Bilibili) and caching it per song.
    Concurrent requests for the same song share one fetch; failed fetches are not
    cached, so a later request retries. *)
val get : t -> Song.t -> string Deferred.Or_error.t
