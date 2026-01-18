open! Core
open! Async

type t

val create
  :  ssrc:Model.Ssrc.t
  -> ip:string
  -> port:int
  -> encryption_modes:Model.Encryption_mode.t list
  -> send_speaking:(Model.Voice_gateway.Event.Speaking.t -> unit Deferred.t)
  -> t Deferred.Or_error.t

val ssrc : t -> Model.Ssrc.t
val discover_ip : t -> (my_ip:string * my_port:int) Deferred.Or_error.t
val frames_writer : t -> secret_key:bytes -> Audio.Pcm_frame.t Queue.t Pipe.Writer.t
val encryption_mode : t -> Model.Encryption_mode.t
