open! Core

type 'a t

val create : 'a Nonempty_list.t -> 'a t
val next : 'a t -> 'a
val peak : 'a t -> 'a
