open! Core
open! Async

let with_ f =
  Signal.handle Signal.terminating ~f:(fun (_ : Signal.t) -> Shutdown.shutdown 0);
  let shutdown = Ivar.create () in
  let shutdown_finished = Ivar.create () in
  Shutdown.at_shutdown (fun () ->
    Ivar.fill_if_empty shutdown ();
    Ivar.read shutdown_finished);
  let%bind.Deferred ret = f (`Shutdown (Ivar.read shutdown)) in
  let%bind () = Log.flushed (force Log.Global.log) in
  Out_channel.flush Stdlib.stdout;
  Ivar.fill_if_empty shutdown_finished ();
  return ret
;;
