open! Core

type t =
  { max : int
  ; mutable remaining : int
  ; reset_after : Time_ns.Span.t option
  ; mutable last_attempt : Time_ns.t
  }
[@@deriving sexp_of]

let create ?reset_after ~max () =
  assert (max >= 0);
  { max; remaining = max; reset_after; last_attempt = Time_ns.epoch }
;;

let reset t = t.remaining <- t.max

let try_ t =
  let now = Time_ns.now () in
  (match t.reset_after with
   | None -> ()
   | Some reset_after ->
     let reset_after = Time_ns.add_saturating t.last_attempt reset_after in
     if Time_ns.O.(now > reset_after) then reset t);
  t.last_attempt <- now;
  if t.remaining > 0
  then (
    t.remaining <- t.remaining - 1;
    Ok ())
  else Or_error.error_s [%message "Exceeded max attempts" ~max_attempts:(t.max : int)]
;;
