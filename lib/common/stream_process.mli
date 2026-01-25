open! Core
open! Async

val stream
  :  here:[%call_pos]
  -> ?cancellation_token:unit Deferred.t
  -> ?on_error:(string -> unit Deferred.t)
  -> ?stdin:Reader.t
  -> prog:File_path.Absolute.t
  -> args:string list
  -> unit
  -> Reader.t Deferred.Or_error.t
