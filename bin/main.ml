open! Core

let () =
  Memtrace.trace_if_requested ();
  Command_unix.run Yum.Cmd_main.command
;;
