open! Core
open! Async

val with_
  :  ([ `Shutdown of unit Deferred.t ] -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t
