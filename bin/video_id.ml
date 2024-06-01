open! Core

type t = string

let prefix = "https://www.youtube.com/watch?v="
let of_string = Fn.id
let to_url t = [%string "%{prefix}%{t}"]
let of_url_exn t = String.chop_prefix_exn ~prefix t
