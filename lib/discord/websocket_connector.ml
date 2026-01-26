open! Core
open! Async
open! Common

let connect ?(time_source = Time_source.wall_clock ()) uri =
  let attempt = Attempt.create ~max:3 () in
  let retry_after = Time_ns.Span.of_int_sec 5 in
  Deferred.repeat_until_finished () (fun () ->
    match%bind
      Deferred.Or_error.try_with_join (fun () ->
        Cohttp_async_websocket.Client.create' uri)
    with
    | Ok _ as result -> return (`Finished result)
    | Error connection_error ->
      (match Attempt.try_ attempt with
       | Ok () ->
         [%log.error
           [%here]
             [%string "Connection failed, retrying in %{retry_after#Time_ns.Span}..."]
             (attempt : Attempt.t)
             (connection_error : Error.t)];
         let%map () = Time_source.after time_source retry_after in
         `Repeat ()
       | Error attempt_arror ->
         return (`Finished (Error (Error.of_list [ attempt_arror; connection_error ])))))
;;
