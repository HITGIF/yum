open! Ctypes
open! Foreign

module Encoder = struct
  type t = unit ptr

  let t : t typ = ptr void

  let create =
    foreign
      "opus_encoder_create"
      (int (* sample rate *)
       @-> int (* channels *)
       @-> int (* application *)
       @-> ptr int (* error *)
       @-> returning t)
  ;;

  let destroy = foreign "opus_encoder_destroy" (t @-> returning void)

  let encode =
    foreign
      "opus_encode"
      (t
       @-> ocaml_bytes (* pcm *)
       @-> int (* frame size *)
       @-> ocaml_bytes (* data *)
       @-> int (* max data bytes *)
       @-> returning int)
  ;;
end
