open! Core
open! Async

type t

val create
  :  ssrc:Model.Ssrc.t
  -> ip:string
  -> port:int
  -> tls_encryption_modes:Model.Tls_encryption_mode.t list
  -> send_speaking:(Model.Voice_gateway.Event.Speaking.t -> unit Deferred.t)
  -> t Deferred.Or_error.t

val ssrc : t -> Model.Ssrc.t
val discover_ip : t -> (my_ip:string * my_port:int) Deferred.Or_error.t

val frames_writer
  :  t
  -> tls_secret_key:bytes
  -> dave_session:Dave_session.t
  -> Audio.Pcm_frame.t Queue.t Pipe.Writer.t

val tls_encryption_mode : t -> Model.Tls_encryption_mode.t
