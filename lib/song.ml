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
  type t [@@deriving sexp_of]

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

module Youtube : sig
  include S
  module Playlist : S
end = struct
  type t = string [@@deriving sexp_of]

  let prefix_normal = "https://www.youtube.com/watch?v="
  let prefix_short = "https://youtu.be/"
  let prefix_music = "https://music.youtube.com/watch?v="

  let supported_url_formats =
    [ prefix_normal; prefix_short; prefix_music ]
    |> List.map ~f:(fun x -> [%string "[...]%{x}<id>[...]"])
  ;;

  let of_string = Fn.id

  let of_url url =
    let open Option.Let_syntax in
    find_prefix_and_chop url ~prefix:prefix_normal
    =? (fun () -> find_prefix_and_chop url ~prefix:prefix_short)
    =? (fun () -> find_prefix_and_chop url ~prefix:prefix_music)
    >>| Fn.compose List.hd_exn (String.split ~on:'?')
    >>| Fn.compose List.hd_exn (String.split ~on:'&')
    >>| of_string
  ;;

  let to_url t = [%string "%{prefix_normal}%{t}"]

  module Playlist : S = struct
    type t = string [@@deriving sexp_of]

    module With_song = struct
      let video_prefix_normal = "https://www.youtube.com/watch?v="
      let video_prefix_music = "https://music.youtube.com/watch?v="
      let playlist_prefix = "&list="

      let find_prefix_and_chop url =
        let open Option.Let_syntax in
        let%bind.Option id = of_url url in
        find_prefix_and_chop url ~prefix:(video_prefix_normal ^ id ^ playlist_prefix)
        =? fun () ->
        find_prefix_and_chop url ~prefix:(video_prefix_music ^ id ^ playlist_prefix)
      ;;

      let supported_url_formats =
        [ video_prefix_normal; video_prefix_music ]
        |> List.map ~f:(fun x ->
          [%string "[...]%{x}<video-id>%{playlist_prefix}<list-id>[...]"])
      ;;
    end

    let prefix_canonical = "https://www.youtube.com/playlist?list="
    let prefix_music = "https://music.youtube.com/playlist?list="

    let supported_url_formats =
      ([ prefix_canonical; prefix_music ]
       |> List.map ~f:(fun x -> [%string "[...]%{x}<list-id>[...]"]))
      @ With_song.supported_url_formats
    ;;

    let of_string = Fn.id

    let of_url url =
      let open Option.Let_syntax in
      find_prefix_and_chop url ~prefix:prefix_canonical
      =? (fun () -> find_prefix_and_chop url ~prefix:prefix_music)
      =? (fun () -> With_song.find_prefix_and_chop url)
      >>| Fn.compose List.hd_exn (String.split ~on:'?')
      >>| Fn.compose List.hd_exn (String.split ~on:'&')
      >>| of_string
    ;;

    let to_url t = [%string "%{prefix_canonical}%{t}"]
  end
end

module Bilibili : S = struct
  type t =
    { video : string
    ; part : int option
    }
  [@@deriving sexp_of]

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

type t =
  | Youtube of Youtube.t
  | Bilibili of Bilibili.t
[@@deriving variants, sexp_of]

let supported_url_formats_msg =
  [ "> Supported `<url>` formats:" ]
  @ (Youtube.supported_url_formats @ Bilibili.supported_url_formats
     |> List.map ~f:(fun x -> [%string "> - `%{x}`"]))
  |> String.concat ~sep:"\n"
;;

let of_youtube_string = Fn.compose youtube Youtube.of_string

let of_url url =
  let open Option.Let_syntax in
  Fn.compose Option.map ~f:youtube Youtube.of_url url
  =? (fun () -> Fn.compose Option.map ~f:bilibili Bilibili.of_url url)
  |> function
  | Some x -> Ok x
  | None ->
    Or_error.error_string
      [%string "URL format is not supported.\n\n%{supported_url_formats_msg}"]
;;

let to_url = function
  | Youtube id -> Youtube.to_url id
  | Bilibili id -> Bilibili.to_url id
;;

let to_src t =
  let url = to_url t in
  match t with
  | Youtube _ -> `Ytdl url
  | Bilibili _ -> `Bilibili url
;;

module Playlist = struct
  type t = Youtube of Youtube.Playlist.t [@@deriving variants, sexp_of]

  let supported_url_formats_msg =
    [ "> Supported `<playlist-url>` formats:" ]
    @ (Youtube.Playlist.supported_url_formats
       |> List.map ~f:(fun x -> [%string "> - `%{x}`"]))
    |> String.concat ~sep:"\n"
  ;;

  let of_url url =
    Fn.compose Option.map ~f:youtube Youtube.Playlist.of_url url
    |> function
    | Some x -> Ok x
    | None ->
      Or_error.error_string
        [%string "URL format is not supported.\n\n%{supported_url_formats_msg}"]
  ;;

  let to_url = function
    | Youtube id -> Youtube.Playlist.to_url id
  ;;

  let to_src t =
    let url = to_url t in
    match t with
    | Youtube _ -> `Ytdl_playlist url
  ;;

  module%test _ = struct
    let youtube_playlist_urls =
      [ "https://www.youtube.com/playlist?list=OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng"
      ; "https://music.youtube.com/playlist?list=OLAK5uy_keRvyy4dXQMs-X8e6iqFrOJ3rTqbBB5B4"
      ; "https://www.youtube.com/watch?v=IO7aE-MqLzE&list=OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng"
      ; "https://music.youtube.com/watch?v=mc4QB6YylFo&list=OLAK5uy_keRvyy4dXQMs-X8e6iqFrOJ3rTqbBB5B4"
      ; "https://www.youtube.com/watch?v=hXPRc916iTM&list=OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng&index=12"
      ]
    ;;

    let%expect_test "youtube playlist url normalization" =
      let test url =
        Youtube.Playlist.(url |> of_url |> Option.value_exn |> to_url |> print_endline)
      in
      List.iter youtube_playlist_urls ~f:test;
      [%expect
        {|
        https://www.youtube.com/playlist?list=OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng
        https://www.youtube.com/playlist?list=OLAK5uy_keRvyy4dXQMs-X8e6iqFrOJ3rTqbBB5B4
        https://www.youtube.com/playlist?list=OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng
        https://www.youtube.com/playlist?list=OLAK5uy_keRvyy4dXQMs-X8e6iqFrOJ3rTqbBB5B4
        https://www.youtube.com/playlist?list=OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng |}]
    ;;

    let%expect_test "of_url" =
      let test url = of_url url |> Or_error.sexp_of_t sexp_of_t |> print_s in
      youtube_playlist_urls @ [ ""; "aaa"; "kaln  klsjlkaj " ] |> List.iter ~f:test;
      [%expect
        {|
        (Ok (Youtube OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng))
        (Ok (Youtube OLAK5uy_keRvyy4dXQMs-X8e6iqFrOJ3rTqbBB5B4))
        (Ok (Youtube OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng))
        (Ok (Youtube OLAK5uy_keRvyy4dXQMs-X8e6iqFrOJ3rTqbBB5B4))
        (Ok (Youtube OLAK5uy_kbVdQzB5u4ZFLvq7mp1dZkD6xMVQ4Hkng))
        (Error
          "URL format is not supported.\
         \n\
         \n> Supported `<playlist-url>` formats:\
         \n> - `[...]https://www.youtube.com/playlist?list=<list-id>[...]`\
         \n> - `[...]https://music.youtube.com/playlist?list=<list-id>[...]`\
         \n> - `[...]https://www.youtube.com/watch?v=<video-id>&list=<list-id>[...]`\
         \n> - `[...]https://music.youtube.com/watch?v=<video-id>&list=<list-id>[...]`")
        (Error
          "URL format is not supported.\
         \n\
         \n> Supported `<playlist-url>` formats:\
         \n> - `[...]https://www.youtube.com/playlist?list=<list-id>[...]`\
         \n> - `[...]https://music.youtube.com/playlist?list=<list-id>[...]`\
         \n> - `[...]https://www.youtube.com/watch?v=<video-id>&list=<list-id>[...]`\
         \n> - `[...]https://music.youtube.com/watch?v=<video-id>&list=<list-id>[...]`")
        (Error
          "URL format is not supported.\
         \n\
         \n> Supported `<playlist-url>` formats:\
         \n> - `[...]https://www.youtube.com/playlist?list=<list-id>[...]`\
         \n> - `[...]https://music.youtube.com/playlist?list=<list-id>[...]`\
         \n> - `[...]https://www.youtube.com/watch?v=<video-id>&list=<list-id>[...]`\
         \n> - `[...]https://music.youtube.com/watch?v=<video-id>&list=<list-id>[...]`") |}]
    ;;
  end
end

module%test _ = struct
  let youtube_urls =
    [ "https://www.youtube.com/watch?v=U7L-3VXAkSA"
    ; "https://www.youtube.com/watch?v=oXZcuHIR5ko&pp=ygUJbG92ZSAyMDAw"
    ; "https://music.youtube.com/watch?v=FojYi2Qfi7c&si=ZiULf0fu65Ror8t8"
    ; "   https://www.youtube.com/watch?v=oXZcuHIR5ko&pp=ygUJbG92ZSAyMDAw  "
    ; " https://youtu.be/H767hjCLk5A"
    ; " https://youtu.be/EUsG3oY4Cmo?si=x6u51-6NfhX5-okB "
    ; "https://www.youtube.com/watch?v=qzoq8cpp6qI&list=LL&index=1"
    ; "sakdlj lakdj lk https://www.youtube.com/watch?v=qzoq8cpp6qI&list=LL&index=1"
    ]
  ;;

  let bilibili_urls =
    [ "https://www.bilibili.com/video/BV1ez421X7Gz/?spm_id_from=333.999.0.0&vd_source=9e41e603371977727f00ca56687eaa1e"
    ; "https://www.bilibili.com/video/BV1ez421X7Gz/?share_source=copy_web&vd_source=3e826ee3881d478c81bbfea52fdc92d6&t=153"
    ; "【【异世界情绪】日文翻唱《死ぬのがいいわ/不如死去》】 \
       https://www.bilibili.com/video/BV1ez421X7Gz/?share_source=copy_web&vd_source=3e826ee3881d478c81bbfea52fdc92d6"
    ; "【【异世界情绪】CH4NGE【Candy Live2】】 【精准空降到 00:03】 \
       https://www.bilibili.com/video/BV1S14y1T7uj/?share_source=copy_web&vd_source=3e826ee3881d478c81bbfea52fdc92d6&t=3"
    ; "https://b23.tv/9XSgbpt"
    ; "https://www.bilibili.com/video/BV1nC8CetEeS/?vd_source=9e41e603371977727f00ca56687eaa1e&p=3"
    ; "https://www.bilibili.com/video/BV1nC8CetEeS?p=8&vd_source=9e41e603371977727f00ca56687eaa1e"
    ; "https://www.bilibili.com/video/BV1nC8CetEeS?p=14&vd_source=9e41e603371977727f00ca56687eaa1e"
    ; "https://www.bilibili.com/video/BV1nC8CetEeS/?p=14&vd_source=9e41e603371977727f00ca56687eaa1e"
    ; "https://www.bilibili.com/video/BV1nC8CetEeS/?p=7"
    ]
  ;;

  let%expect_test "youtube url normalization" =
    let test url =
      Youtube.(url |> of_url |> Option.value_exn |> to_url |> print_endline)
    in
    List.iter youtube_urls ~f:test;
    [%expect
      {|
        https://www.youtube.com/watch?v=U7L-3VXAkSA
        https://www.youtube.com/watch?v=oXZcuHIR5ko
        https://www.youtube.com/watch?v=FojYi2Qfi7c
        https://www.youtube.com/watch?v=oXZcuHIR5ko
        https://www.youtube.com/watch?v=H767hjCLk5A
        https://www.youtube.com/watch?v=EUsG3oY4Cmo
        https://www.youtube.com/watch?v=qzoq8cpp6qI
        https://www.youtube.com/watch?v=qzoq8cpp6qI |}]
  ;;

  let%expect_test "bilibili url normalization" =
    let test url =
      Bilibili.(url |> of_url |> Option.value_exn |> to_url |> print_endline)
    in
    List.iter bilibili_urls ~f:test;
    [%expect
      {|
        https://www.bilibili.com/video/BV1ez421X7Gz
        https://www.bilibili.com/video/BV1ez421X7Gz
        https://www.bilibili.com/video/BV1ez421X7Gz
        https://www.bilibili.com/video/BV1S14y1T7uj
        https://www.bilibili.com/video/BV1wGvUeXEkr
        https://www.bilibili.com/video/BV1nC8CetEeS/?p=3
        https://www.bilibili.com/video/BV1nC8CetEeS/?p=8
        https://www.bilibili.com/video/BV1nC8CetEeS/?p=14
        https://www.bilibili.com/video/BV1nC8CetEeS/?p=14
        https://www.bilibili.com/video/BV1nC8CetEeS/?p=7 |}]
  ;;

  let%expect_test "of_url" =
    let test url = of_url url |> Or_error.sexp_of_t sexp_of_t |> print_s in
    youtube_urls @ bilibili_urls @ [ ""; "aaa"; "kaln  klsjlkaj " ] |> List.iter ~f:test;
    [%expect
      {|
        (Ok (Youtube U7L-3VXAkSA))
        (Ok (Youtube oXZcuHIR5ko))
        (Ok (Youtube FojYi2Qfi7c))
        (Ok (Youtube oXZcuHIR5ko))
        (Ok (Youtube H767hjCLk5A))
        (Ok (Youtube EUsG3oY4Cmo))
        (Ok (Youtube qzoq8cpp6qI))
        (Ok (Youtube qzoq8cpp6qI))
        (Ok (Bilibili ((video BV1ez421X7Gz) (part ()))))
        (Ok (Bilibili ((video BV1ez421X7Gz) (part ()))))
        (Ok (Bilibili ((video BV1ez421X7Gz) (part ()))))
        (Ok (Bilibili ((video BV1S14y1T7uj) (part ()))))
        (Ok (Bilibili ((video BV1wGvUeXEkr) (part ()))))
        (Ok (Bilibili ((video BV1nC8CetEeS) (part (3)))))
        (Ok (Bilibili ((video BV1nC8CetEeS) (part (8)))))
        (Ok (Bilibili ((video BV1nC8CetEeS) (part (14)))))
        (Ok (Bilibili ((video BV1nC8CetEeS) (part (14)))))
        (Ok (Bilibili ((video BV1nC8CetEeS) (part (7)))))
        (Error
          "URL format is not supported.\
         \n\
         \n> Supported `<url>` formats:\
         \n> - `[...]https://www.youtube.com/watch?v=<id>[...]`\
         \n> - `[...]https://youtu.be/<id>[...]`\
         \n> - `[...]https://music.youtube.com/watch?v=<id>[...]`\
         \n> - `[...]https://www.bilibili.com/video/<id>[...]`\
         \n> - `[...]https://b23.tv/<id>[...]`")
        (Error
          "URL format is not supported.\
         \n\
         \n> Supported `<url>` formats:\
         \n> - `[...]https://www.youtube.com/watch?v=<id>[...]`\
         \n> - `[...]https://youtu.be/<id>[...]`\
         \n> - `[...]https://music.youtube.com/watch?v=<id>[...]`\
         \n> - `[...]https://www.bilibili.com/video/<id>[...]`\
         \n> - `[...]https://b23.tv/<id>[...]`")
        (Error
          "URL format is not supported.\
         \n\
         \n> Supported `<url>` formats:\
         \n> - `[...]https://www.youtube.com/watch?v=<id>[...]`\
         \n> - `[...]https://youtu.be/<id>[...]`\
         \n> - `[...]https://music.youtube.com/watch?v=<id>[...]`\
         \n> - `[...]https://www.bilibili.com/video/<id>[...]`\
         \n> - `[...]https://b23.tv/<id>[...]`") |}]
  ;;
end
