open! Core
open! Async
open! Common

let default_prog = File_path.Absolute.of_string "/usr/bin/media-get"
let () = Todo.support_bilibili
