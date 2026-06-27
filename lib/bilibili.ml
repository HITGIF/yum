open! Core
open! Async

let headers =
  Cohttp.Header.of_list
    [ ( "user-agent"
      , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like \
         Gecko) Chrome/96.0.4664.110 Safari/537.36" )
      (* Bilibili's video CDN returns 403 without a bilibili referer, and the
         json APIs are friendlier with one too. *)
    ; "referer", "https://www.bilibili.com"
    ]
;;

let validate_response ~(here : [%call_pos]) response =
  match Cohttp.Response.status response with
  | `OK -> Ok ()
  | status_code ->
    Or_error.error_s
      [%message
        "Unexpected HTTP response code"
          (status_code : Cohttp.Code.status_code)
          (here : Source_code_position.t)]
;;

(* GET [url], validate the HTTP status, then parse the body as a bilibili json
   API envelope: [{ "code": 0, "message": "OK", "data": ... }]. A non-zero
   [code] is an application-level error even though the HTTP status is 200. *)
let get_json ~url ~of_yojson =
  let%bind response, body = Cohttp_async.Client.get ~headers url in
  let%bind.Deferred.Or_error () =
    validate_response response
    |> Or_error.tag_s_lazy ~tag:[%lazy_sexp { url : string = Uri.to_string url }]
    |> return
  in
  let%map body = Cohttp_async.Body.to_string body in
  Or_error.try_with (fun () ->
    let json = Yojson.Safe.from_string body in
    let member name = Yojson.Safe.Util.member name json in
    match member "code" |> Yojson.Safe.Util.to_int with
    | 0 -> member "data" |> of_yojson
    | code ->
      raise_s
        [%message
          "Bilibili API returned an error"
            (code : int)
            ~message:(member "message" |> Yojson.Safe.Util.to_string : string)
            ~url:(Uri.to_string url : string)])
  |> Or_error.tag_s_lazy
       ~tag:[%lazy_sexp { url : string = Uri.to_string url }]
;;

module Api = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  (* https://api.bilibili.com/x/web-interface/view?bvid=... *)
  module View = struct
    type page =
      { cid : int
      ; page : int
      }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson]

    type t =
      { cid : int (* cid of the first page *)
      ; pages : page list
      }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson]
  end

  (* https://api.bilibili.com/x/player/playurl?bvid=...&cid=...&fnval=16 *)
  module Playurl = struct
    type audio =
      { base_url : string [@key "baseUrl"]
      ; backup_url : string list [@key "backupUrl"] [@default []]
      ; bandwidth : int
      }
    [@@yojson.allow_extra_fields] [@@deriving sexp_of, of_yojson, fields ~getters]

    let urls { base_url; backup_url; _ } = base_url :: backup_url

    type dash = { audio : audio list }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson]

    type t = { dash : dash } [@@yojson.allow_extra_fields] [@@deriving of_yojson]
  end
end

(* Resolve the [cid] (the id bilibili uses to identify a specific playable part)
   for the requested [part], defaulting to the first part. *)
let get_cid ~bvid ~part =
  let url =
    Uri.of_string
      [%string "https://api.bilibili.com/x/web-interface/view?bvid=%{bvid}"]
  in
  let%map.Deferred.Or_error view = get_json ~url ~of_yojson:Api.View.t_of_yojson in
  match part with
  | None | Some 1 -> view.cid
  | Some part ->
    (match List.find view.pages ~f:(fun page -> page.page = part) with
     | Some page -> page.cid
     | None -> view.cid)
;;

(* highest bandwidth first, falling back to backup mirror urls *)
let best_audio_urls (audios : Api.Playurl.audio list) =
  List.sort
    audios
    ~compare:
      (Comparable.lift
         (Comparable.reverse [%compare: int])
         ~f:Api.Playurl.bandwidth)
  |> List.concat_map ~f:Api.Playurl.urls
  |> List.map ~f:Uri.of_string
;;

let get_audio_urls ~bvid ~cid =
  let url =
    Uri.of_string
      [%string
        "https://api.bilibili.com/x/player/playurl?bvid=%{bvid}&cid=%{cid#Int}&fnval=16"]
  in
  let%bind.Deferred.Or_error playurl =
    get_json ~url ~of_yojson:Api.Playurl.t_of_yojson
  in
  match playurl.dash.audio with
  | [] -> Deferred.Or_error.error_s [%message "No audio streams found" bvid (cid : int)]
  | audios -> best_audio_urls audios |> Deferred.Or_error.return
;;

let download_audio audio_urls =
  let%bind.Deferred.Or_error body, audio_url =
    Deferred.List.fold_until
      audio_urls
      ~init:[]
      ~f:(fun acc audio_url ->
        let%map response, body = Cohttp_async.Client.get ~headers audio_url in
        match
          validate_response response
          |> Or_error.tag_s_lazy
               ~tag:[%lazy_sexp { audio_url : string = Uri.to_string audio_url }]
        with
        | Ok () -> Continue_or_stop.Stop (Ok (body, audio_url))
        | Error error -> Continue (error :: acc))
      ~finish:(fun errors -> Deferred.Or_error.fail (Error.of_list errors))
  in
  Reader.of_pipe
    (Info.create_s [%message "Audio response body" ~_:(Uri.to_string audio_url : string)])
    (Cohttp_async.Body.to_pipe body)
  |> Deferred.ok
;;

(* [video] is the BV id; [part] is the optional 1-based part number. *)
let download ~video:bvid ~part =
  let%bind.Deferred.Or_error cid = get_cid ~bvid ~part in
  let%bind.Deferred.Or_error audio_urls = get_audio_urls ~bvid ~cid in
  download_audio audio_urls
;;
