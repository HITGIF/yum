open! Core
open! Async

val download : url:string -> Reader.t Deferred.Or_error.t
