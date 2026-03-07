open! Core

type 'a t =
  { elements : 'a Nonempty_list.t
  ; queue : 'a Queue.t
  }

let enqueue t =
  Nonempty_list.to_list t.elements |> List.permute |> Queue.enqueue_all t.queue
;;

let create elements =
  let t = { elements; queue = Queue.create () } in
  enqueue t;
  t
;;

let rec next t =
  match Queue.dequeue t.queue with
  | Some element -> element
  | None ->
    enqueue t;
    next t
;;

let rec peak t =
  match Queue.peek t.queue with
  | Some element -> element
  | None ->
    enqueue t;
    peak t
;;
