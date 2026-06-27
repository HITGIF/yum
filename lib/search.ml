open! Core
open! Async

(* Discord allows at most 25 options in a string-select, and 100 characters per
   option label/description. *)
let max_results = 25
let max_field_length = 100

module Result = struct
  type t =
    { song : Song.t
    ; label : string
    ; description : string
    }
  [@@deriving sexp_of, fields ~getters]
end

(* Byte length of one UTF-8 codepoint from its leading byte. *)
let utf8_width c =
  match Char.to_int c with
  | c when c < 0x80 -> 1
  | c when c < 0xe0 -> 2
  | c when c < 0xf0 -> 3
  | _ -> 4
;;

(* Byte index just past the first [chars] codepoints (or the whole string). *)
let utf8_prefix_len s ~chars =
  let len = String.length s in
  let rec loop i n =
    if i >= len || n >= chars then i else loop (i + utf8_width s.[i]) (n + 1)
  in
  loop 0 0
;;

(* Discord caps labels/descriptions at 100 characters and rejects the entire
   payload as "invalid JSON" if any string contains malformed UTF-8 — so cut on
   codepoint boundaries, never mid-character. *)
let truncate s =
  if utf8_prefix_len s ~chars:max_field_length >= String.length s
  then s
  else String.prefix s (utf8_prefix_len s ~chars:(max_field_length - 1)) ^ "…"
;;

(* A non-empty label is required by Discord; fall back to the id if a result
   somehow has an empty title. *)
let label ~fallback title =
  match String.strip title with
  | "" -> fallback
  | title -> truncate title
;;

let description parts =
  List.filter_map parts ~f:Fn.id |> String.concat ~sep:" · " |> truncate
;;

(* A missing duration means the entry isn't a playable video (channel, playlist,
   mix, ...), so drop it. *)
let search_youtube ~yt_dlp_path ~query =
  let%map.Deferred.Or_error results =
    Youtube.search ~prog:yt_dlp_path ~max_results query
  in
  List.filter_map
    results
    ~f:(fun { Youtube.Search_result.id; title; uploader; duration } ->
      let%map.Option duration in
      { Result.song = Song.of_youtube_string id
      ; label = label ~fallback:id title
      ; description = description [ uploader; Some duration ]
      })
;;

let search_bilibili ~sessdata ~query =
  let%map.Deferred.Or_error results = Bilibili.search ?sessdata ~max_results query in
  List.filter_map
    results
    ~f:(fun { Bilibili.Search_result.bvid; title; author; duration } ->
      let%map.Option duration in
      { Result.song = Song.of_bilibili_string bvid
      ; label = label ~fallback:bvid title
      ; description = description [ author; Some duration ]
      })
;;

(* Interleave so both platforms are represented even when one returns far more. *)
let rec interleave xs ys =
  match xs, ys with
  | [], rest | rest, [] -> rest
  | x :: xs, y :: ys -> x :: y :: interleave xs ys
;;

(* A platform's own ranking is its best relevance signal, but the two aren't
   comparable across sources. The one signal we can compute uniformly is how well
   the query text matches a result, so we score each result by query/title (and
   author) overlap and sort by it. The score is coarse on purpose: a [stable_sort]
   then keeps the original interleaving among equally-matching results, so
   platform order still breaks ties and both sources stay represented. *)
let relevance ~query { Result.label; description; _ } =
  let haystack = String.lowercase (label ^ " " ^ description) in
  let query = String.lowercase (String.strip query) in
  let tokens = String.split query ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
  let token_hits =
    List.count tokens ~f:(fun token -> String.is_substring haystack ~substring:token)
  in
  let token_fraction =
    match tokens with
    | [] -> 0.
    | _ -> Float.of_int token_hits /. Float.of_int (List.length tokens)
  in
  let phrase = if String.is_substring haystack ~substring:query then 1. else 0. in
  let prefix = if String.is_prefix haystack ~prefix:query then 1. else 0. in
  (* All query tokens present matters most; a verbatim phrase / title prefix is a
     strong extra signal. *)
  (token_fraction *. 2.) +. phrase +. prefix
;;

let rank_by_relevance ~query results =
  List.stable_sort results ~compare:(fun a b ->
    Float.compare (relevance ~query b) (relevance ~query a))
;;

(* Search YouTube and Bilibili concurrently and merge. One source failing (e.g.
   Bilibili risk control) doesn't fail the whole search — we return the other's
   results; only if both fail do we surface an error. *)
let search ?bilibili_sessdata ~yt_dlp_path ~query () =
  let%map.Deferred youtube = search_youtube ~yt_dlp_path ~query
  and bilibili = search_bilibili ~sessdata:bilibili_sessdata ~query in
  match youtube, bilibili with
  | Ok youtube, Ok bilibili ->
    Ok
      (interleave youtube bilibili
       |> rank_by_relevance ~query
       |> fun r -> List.take r max_results)
  | Ok results, Error _ | Error _, Ok results -> Ok (List.take results max_results)
  | Error youtube, Error bilibili -> Error (Error.of_list [ youtube; bilibili ])
;;

module%test _ = struct
  let count_chars s =
    let len = String.length s in
    let rec loop i n = if i >= len then n else loop (i + utf8_width s.[i]) (n + 1) in
    loop 0 0
  ;;

  let%expect_test "truncate cuts on codepoint boundaries" =
    (* 150 three-byte characters; byte-truncation would split the 100th char and
       produce malformed UTF-8 (the bug Discord rejected as invalid JSON). *)
    let cjk = String.concat (List.init 150 ~f:(Fn.const "あ")) in
    let truncated = truncate cjk in
    printf
      "chars=%d valid_utf8=%b ends_with_ellipsis=%b short_unchanged=%b\n"
      (count_chars truncated)
      (Stdlib.String.is_valid_utf_8 truncated)
      (String.is_suffix truncated ~suffix:"…")
      (String.equal (truncate "hello") "hello");
    [%expect {| chars=100 valid_utf8=true ends_with_ellipsis=true short_unchanged=true |}];
    return ()
  ;;

  let%expect_test "rank_by_relevance floats query matches up, stable otherwise" =
    let result label =
      { Result.song = Song.of_youtube_string "x"; label; description = "" }
    in
    (* Interleaved input: a strong match sits low; unrelated items sit high. *)
    [ result "random video"
    ; result "another clip"
    ; result "asu kamitsubaki live"
    ; result "asu cover"
    ]
    |> rank_by_relevance ~query:"asu kamitsubaki"
    |> List.iter ~f:(fun { Result.label; _ } -> print_endline label);
    [%expect
      {|
      asu kamitsubaki live
      asu cover
      random video
      another clip
      |}];
    return ()
  ;;
end
