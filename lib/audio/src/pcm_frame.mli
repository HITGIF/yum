open! Core
open! Async

type t

val channels : int
val frame_duration : Time_ns.Span.t

(** samples per second *)
val sample_rate : int

(** a.k.a. frame size *)
val samples_per_frame : int

val ffmpeg_format : string

(*_ *)
val read : Reader.t -> t Pipe.Reader.t
val to_bytes : t -> bytes
