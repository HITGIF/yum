open! Core
open! Async

val download : Uri.t -> Reader.t Deferred.Or_error.t
