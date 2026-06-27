open! Core
open! Async

(** [download ~video ~part] streams the highest-bandwidth audio for the given
    bilibili [video] (a BV id) and optional 1-based [part]. *)
val download : video:string -> part:int option -> Reader.t Deferred.Or_error.t
