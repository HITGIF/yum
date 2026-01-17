open! Core
open! Ctypes
open! Bindings_core
open Opus_bindings.Encoder

module Frame = struct
  type t = bytes

  let silence = Bytes.of_string "\xf8\xff\xfe"
  let to_bytes = Fn.id
end

module Encoder = struct
  type nonrec t = t

  let create ~application =
    let%bind.Or_error () =
      if Sys.big_endian
      then
        Or_error.error_s
          [%message [%here] [%module_name] "Big endian machines are not supported"]
      else Ok ()
    in
    let application =
      match application with
      | Application.Voip -> 2048
      | Audio -> 2049
      | RestrictedLowdelay -> 2051
    in
    let error = allocate int 0 in
    let t = create Pcm_frame.sample_rate Pcm_frame.channels application error in
    match !@error with
    | 0 -> Ok t
    | errno ->
      Or_error.error_s
        [%message [%here] [%module_name] "encoder creation failed" (errno : int)]
  ;;

  let destroy = destroy

  let encode t pcm =
    let pcm = Pcm_frame.to_bytes pcm in
    let len = Bytes.length pcm in
    let data = Bytes.create len in
    match encode t !!pcm Pcm_frame.samples_per_frame !!data len with
    | errno when errno < 0 ->
      Or_error.error_s [%message [%here] [%module_name] "encoding failed" (errno : int)]
    | len -> Bytes.subo data ~len |> Ok
  ;;
end
