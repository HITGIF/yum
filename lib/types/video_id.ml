open! Core

module Option = struct
  include Option

  let or_else ~f = function
    | Some x -> Some x
    | None -> f ()
  ;;

  module Let_syntax = struct
    include Let_syntax

    let ( =? ) t f = or_else t ~f
  end
end

module type S = sig
  type t

  val supported_url_formats : string list
  val of_string : string -> t
  val of_url : string -> t option
  val to_url : t -> string
end

let find_prefix_and s ~prefix ~f =
  let open Option.Let_syntax in
  String.substr_index s ~pattern:prefix >>| (fun pos -> String.subo s ~pos) >>= f
;;

let find_prefix_and_chop s ~prefix =
  find_prefix_and s ~prefix ~f:(String.chop_prefix ~prefix)
;;

let find_prefix_and_curl =
  find_prefix_and ~f:(fun url ->
    Core_unix.create_process ~prog:"curl" ~args:[ url ]
    |> (fun { Core_unix.Process_info.stdout; _ } -> stdout)
    |> Core_unix.in_channel_of_descr
    |> In_channel.input_all
    |> Option.return)
;;

module Youtube : S = struct
  type t = string

  let prefix_normal = "https://www.youtube.com/watch?v="
  let prefix_short = "https://youtu.be/"

  let supported_url_formats =
    [ prefix_normal; prefix_short ]
    |> List.map ~f:(fun x -> [%string "[...]%{x}<id>[...]"])
  ;;

  let of_string = Fn.id

  let of_url url =
    let open Option.Let_syntax in
    find_prefix_and_chop url ~prefix:prefix_normal
    =? (fun () -> find_prefix_and_chop url ~prefix:prefix_short)
    >>| Fn.compose List.hd_exn (String.split ~on:'?')
    >>| Fn.compose List.hd_exn (String.split ~on:'&')
    >>| of_string
  ;;

  let to_url t = [%string "%{prefix_normal}%{t}"]
end

module Bilibili : S = struct
  type t =
    { video : string
    ; part : int option
    }

  let prefix_normal = "https://www.bilibili.com/video/"
  let prefix_short = "https://b23.tv/"

  let supported_url_formats =
    [ prefix_normal; prefix_short ]
    |> List.map ~f:(fun x -> [%string "[...]%{x}<id>[...]"])
  ;;

  let of_string s = { video = s; part = None }

  let to_string { video; part } =
    match part with
    | None -> video
    | Some part -> [%string "%{video}/?p=%{part#Int}"]
  ;;

  let of_url url =
    let open Option.Let_syntax in
    let url = String.strip url in
    find_prefix_and_chop url ~prefix:prefix_normal
    =? (fun () ->
         find_prefix_and_curl ~prefix:prefix_short url
         >>= find_prefix_and_chop ~prefix:prefix_normal)
    >>| String.split ~on:'?'
    >>| function
    | [] -> failwith "unreachable"
    | [ id_with_slash ] ->
      Fn.compose List.hd_exn (String.split ~on:'/') id_with_slash |> of_string
    | id_with_slash :: params :: _ ->
      let video = Fn.compose List.hd_exn (String.split ~on:'/') id_with_slash in
      let params = String.split params ~on:'&' in
      (match List.find params ~f:(String.is_prefix ~prefix:"p=") with
       | None -> of_string video
       | Some part ->
         { video; part = String.chop_prefix ~prefix:"p=" part >>| Int.of_string })
  ;;

  let to_url t = [%string "%{prefix_normal}%{to_string t}"]
end

type t = Youtube of Youtube.t [@@deriving variants]

let supported_url_formats_msg =
  [ "> Supported `<url>` formats:" ]
  @ (Youtube.supported_url_formats @ Bilibili.supported_url_formats
     |> List.map ~f:(fun x -> [%string "> - `%{x}`"]))
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
      [%expect {| https://www.youtube.com/watch?v=qzoq8cpp6qI |}];
      test "sakdlj lakdj lk https://www.youtube.com/watch?v=qzoq8cpp6qI&list=LL&index=1";
      [%expect {| https://www.youtube.com/watch?v=qzoq8cpp6qI |}]
    ;;

    let%expect_test "bilibili url normalization" =
      let test url =
        Bilibili.(url |> of_url |> Option.value_exn |> to_url |> print_endline)
      in
      test
        "https://www.bilibili.com/video/BV1ez421X7Gz/?spm_id_from=333.999.0.0&vd_source=9e41e603371977727f00ca56687eaa1e";
      [%expect {| https://www.bilibili.com/video/BV1ez421X7Gz |}];
      test
        "https://www.bilibili.com/video/BV1ez421X7Gz/?share_source=copy_web&vd_source=3e826ee3881d478c81bbfea52fdc92d6&t=153";
      [%expect {| https://www.bilibili.com/video/BV1ez421X7Gz |}];
      test
        "【【异世界情绪】日文翻唱《死ぬのがいいわ/不如死去》】 \
         https://www.bilibili.com/video/BV1ez421X7Gz/?share_source=copy_web&vd_source=3e826ee3881d478c81bbfea52fdc92d6";
      [%expect {| https://www.bilibili.com/video/BV1ez421X7Gz |}];
      test
        "【【异世界情绪】CH4NGE【Candy Live2】】 【精准空降到 00:03】 \
         https://www.bilibili.com/video/BV1S14y1T7uj/?share_source=copy_web&vd_source=3e826ee3881d478c81bbfea52fdc92d6&t=3";
      [%expect {| https://www.bilibili.com/video/BV1S14y1T7uj |}];
      test "https://b23.tv/9XSgbpt";
      [%expect {| https://www.bilibili.com/video/BV1wGvUeXEkr |}];
      test
        "https://www.bilibili.com/video/BV1nC8CetEeS/?vd_source=9e41e603371977727f00ca56687eaa1e&p=3";
      [%expect {| https://www.bilibili.com/video/BV1nC8CetEeS/?p=3 |}];
      test
        "https://www.bilibili.com/video/BV1nC8CetEeS?p=8&vd_source=9e41e603371977727f00ca56687eaa1e";
      [%expect {| https://www.bilibili.com/video/BV1nC8CetEeS/?p=8 |}];
      test
        "https://www.bilibili.com/video/BV1nC8CetEeS?p=14&vd_source=9e41e603371977727f00ca56687eaa1e";
      [%expect {| https://www.bilibili.com/video/BV1nC8CetEeS/?p=14 |}];
      test
        "https://www.bilibili.com/video/BV1nC8CetEeS/?p=14&vd_source=9e41e603371977727f00ca56687eaa1e";
      [%expect {| https://www.bilibili.com/video/BV1nC8CetEeS/?p=14 |}];
      test "https://www.bilibili.com/video/BV1nC8CetEeS/?p=7";
      [%expect {| https://www.bilibili.com/video/BV1nC8CetEeS/?p=7 |}]
    ;;
  end)
;;
