open! Core
open! Async

type 'a t =
  { reader : 'a Pipe.Reader.t
  ; writer : 'a Pipe.Writer.t
  }

let create () =
  let reader, writer = Pipe.create () in
  { reader; writer }
;;

let queue t element = Pipe.write_without_pushback_if_open t.writer element

let dequeue t =
  match%map Pipe.read t.reader with
  | `Eof -> `Closed
  | `Ok element -> `Ok element
;;

let close t = Pipe.close_read t.reader
