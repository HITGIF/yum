open! Core
open! Async

val stream
  :  here:[%call_pos]
  -> ?cancellation_token:unit Deferred.t
  -> ?on_finish:((unit, string) result -> unit Deferred.t)
  -> ?stdin:Reader.t
  -> prog:File_path.Absolute.t
  -> args:string list
  -> unit
  -> Reader.t Deferred.Or_error.t
