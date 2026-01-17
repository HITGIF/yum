open! Core
open! Async

type 'a t

val create : unit -> 'a t
val queue : 'a t -> 'a -> unit
val dequeue : 'a t -> [ `Closed | `Ok of 'a ] Deferred.t
val close : 'a t -> unit
