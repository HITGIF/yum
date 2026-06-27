open! Core
open! Async
open! Common

type t =
  { yt_dlp_path : File_path.Absolute.t
  ; bilibili_sessdata : string option
  ; (* Keyed on the canonical URL so the same song shares one entry. The cached
       value is the in-flight (or settled) fetch, so concurrent requests dedupe. *)
    cache : string Or_error.t Deferred.t String.Table.t
  }

let create ~yt_dlp_path ~bilibili_sessdata =
  { yt_dlp_path; bilibili_sessdata; cache = String.Table.create () }
;;

let fetch t song =
  let url = Song.to_url song in
  let start = Time_ns.now () in
  [%log.info [%here] "Fetching title" (url : string)];
  let%map result =
    match Song.to_src song with
    | `Youtube url -> Youtube.get_title ~prog:t.yt_dlp_path url
    | `Bilibili (bvid, _part) ->
      Bilibili.get_title ?sessdata:t.bilibili_sessdata ~bvid ()
  in
  let elapsed = Time_ns.diff (Time_ns.now ()) start in
  (match result with
   | Ok title ->
     [%log.info
       [%here] "Fetched title" (url : string) (title : string) (elapsed : Time_ns.Span.t)]
   | Error error ->
     [%log.error
       [%here]
         "Failed to fetch title"
         (url : string)
         (elapsed : Time_ns.Span.t)
         (error : Error.t)]);
  result
;;

let get t song =
  let key = Song.to_url song in
  match Hashtbl.find t.cache key with
  | Some title -> title
  | None ->
    let title = fetch t song in
    Hashtbl.set t.cache ~key ~data:title;
    (* Don't let a transient failure (network, risk control) poison the title
       forever: drop a failed fetch so a later request retries it. *)
    upon title (function
      | Error _ -> Hashtbl.remove t.cache key
      | Ok _ -> ());
    title
;;
