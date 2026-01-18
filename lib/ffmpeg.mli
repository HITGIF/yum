open! Core
open! Async

val default_prog : File_path.Absolute.t

val encode_pcm
  :  ?cancellation_token:unit Deferred.t
  -> ?prog:File_path.Absolute.t
  -> ?bitrate:string (* (default: 128k) *)
  -> Reader.t
  -> Audio.Pcm_frame.t Pipe.Reader.t Deferred.Or_error.t
