open! Core
open! Async

let stream
  ~(here : [%call_pos])
  ?(cancellation_token = Deferred.never ())
  ?stdin
  ~prog
  ~args
  ()
  =
  let with_stdin f =
    match stdin with
    | None -> return ()
    | Some stdin -> f stdin
  in
  let%map.Deferred.Or_error process =
    Process.create ~prog:(File_path.Absolute.to_string prog) ~args ()
  in
  don't_wait_for
    (let done_transfering =
       let%with stdin = with_stdin in
       Process.stdin process |> Writer.pipe |> Reader.transfer stdin
     in
     let%bind () = Deferred.any_unit [ done_transfering; cancellation_token ] in
     let%bind () =
       let%with stdin = with_stdin in
       Reader.close stdin
     in
     let%bind () = Process.stdin process |> Writer.close in
     match%bind Process.wait process with
     | Ok () ->
       let%bind () = Process.stderr process |> Reader.close in
       [%log.debug "Process finished" (here : Source_code_position.t)];
       return ()
     | Error error ->
       let%map stderr = Process.stderr process |> Reader.contents in
       [%log.error
         "Process finished with error"
           (here : Source_code_position.t)
           (error : Core_unix.Exit_or_signal.error)
           (stderr : string)]);
  Process.stdout process
;;
