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

module Dave_encrypt : sig
  type t

  val create
    :  encrypt:(frame:bytes -> output:bytes -> Dave.Encryptor_result_code.t * int)
    -> get_max_ciphertext_byte_size:(frame_size:int -> int)
    -> t
end

val frames_writer
  :  t
  -> secret_key:bytes
  -> ?dave_encrypt:Dave_encrypt.t
  -> unit
  -> Audio.Pcm_frame.t Queue.t Pipe.Writer.t

val encryption_mode : t -> Model.Encryption_mode.t
