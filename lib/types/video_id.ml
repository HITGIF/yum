open! Core

module Option = struct
  include Option

  let or_else ~f = function
    | Some x -> Some x
    | None -> f ()
  ;;
end

module Youtube : sig
  type t

  val supported_url_formats : string list
  val of_string : string -> t
  val of_url : string -> t option
  val to_url : t -> string
end = struct
  type t = string

  let prefix_normal = "https://www.youtube.com/watch?v="
  let prefix_short = "https://youtu.be/"

  let supported_url_formats =
    [ prefix_normal; prefix_short ] |> List.map ~f:(fun x -> [%string "%{x}[...]"])
  ;;

  let of_string = Fn.id

  let of_url url =
    let url = String.strip url in
    String.chop_prefix url ~prefix:prefix_normal
    |> Option.or_else ~f:(fun () -> String.chop_prefix ~prefix:prefix_short url)
    |> Option.map ~f:(Fn.compose List.hd_exn (String.split ~on:'?'))
    |> Option.map ~f:(Fn.compose List.hd_exn (String.split ~on:'&'))
  ;;

  let to_url t = [%string "%{prefix_normal}%{t}"]
end

type t = Youtube of Youtube.t [@@deriving variants]

let supported_url_formats_msg =
  [ "> Supported `<url>` formats:" ]
  @ (Youtube.supported_url_formats |> List.map ~f:(fun x -> [%string "> - `%{x}`"]))
  |> String.concat ~sep:"\n"
;;

let of_youtube_string = Fn.compose youtube Youtube.of_string

let of_url url =
  Fn.compose Option.map ~f:youtube Youtube.of_url url
  |> Option.or_else ~f:(fun () -> None)
  |> function
  | Some x -> Ok x
  | None ->
    Or_error.error_string
      [%string "URL format is not supported.\n\n%{supported_url_formats_msg}"]
;;

let to_url = function
  | Youtube youtube -> Youtube.to_url youtube
;;

let%test_module "_" =
  (module struct
    let%expect_test "youtube url normalization" =
      let test url =
        Youtube.(url |> of_url |> Option.value_exn |> to_url |> print_endline)
      in
      test "https://www.youtube.com/watch?v=U7L-3VXAkSA";
      [%expect {| https://www.youtube.com/watch?v=U7L-3VXAkSA |}];
      test "https://www.youtube.com/watch?v=oXZcuHIR5ko&pp=ygUJbG92ZSAyMDAw";
      [%expect {| https://www.youtube.com/watch?v=oXZcuHIR5ko |}];
      test "   https://www.youtube.com/watch?v=oXZcuHIR5ko&pp=ygUJbG92ZSAyMDAw  ";
      [%expect {| https://www.youtube.com/watch?v=oXZcuHIR5ko |}];
      test " https://youtu.be/H767hjCLk5A";
      [%expect {| https://www.youtube.com/watch?v=H767hjCLk5A |}];
      test " https://youtu.be/EUsG3oY4Cmo?si=x6u51-6NfhX5-okB ";
      [%expect {| https://www.youtube.com/watch?v=EUsG3oY4Cmo |}];
      test "https://www.youtube.com/watch?v=qzoq8cpp6qI&list=LL&index=1";
      [%expect {| https://www.youtube.com/watch?v=qzoq8cpp6qI |}]
    ;;
  end)
;;
