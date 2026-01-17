open! Core

module Frame : sig
  type t

  val silence : t
  val to_bytes : t -> bytes
end

module Encoder : sig
  type t

  val create : application:Application.t -> t Or_error.t
  val destroy : t -> unit
  val encode : t -> Pcm_frame.t -> Frame.t Or_error.t
end
