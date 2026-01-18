open! Core
open! Async

val default_prog : File_path.Absolute.t

val download
  :  ?cancellation_token:unit Deferred.t
  -> ?prog:File_path.Absolute.t
  -> ?args:string list
  -> string
  -> Reader.t Deferred.Or_error.t

val get_playlist
  :  ?prog:File_path.Absolute.t
  -> ?args:string list
  -> string
  -> Song.t list Deferred.Or_error.t
