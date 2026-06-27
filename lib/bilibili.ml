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
let get_json ?(headers = headers) ~url ~of_yojson () =
  let%bind response, body = Cohttp_async.Client.get ~headers url in
  let%bind.Deferred.Or_error () =
    validate_response response
    |> Or_error.tag_s_lazy ~tag:[%lazy_sexp { url : string = Uri.to_string url }]
    |> return
  in
  let%map body = Cohttp_async.Body.to_string body in
  Or_error.try_with (fun () ->
    (* Risk control serves an HTML challenge page instead of JSON; surface that
       clearly rather than as an opaque Yojson parse error. *)
    if String.is_prefix (String.lstrip body) ~prefix:"<"
    then
      raise_s
        [%message
          "Bilibili served an HTML page instead of JSON (likely anti-bot risk \
           control on this IP)"
            ~url:(Uri.to_string url : string)];
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

  (* https://api.bilibili.com/x/frontend/finger/spi -> an anonymous buvid, which
     the search API wants as a [buvid3] cookie to avoid anti-bot rejection. *)
  module Spi = struct
    type t = { b_3 : string } [@@yojson.allow_extra_fields] [@@deriving of_yojson]
  end

  (* https://api.bilibili.com/x/web-interface/search/type?search_type=video&keyword=... *)
  module Search = struct
    type result =
      { bvid : string
      ; title : string
      ; author : string [@default ""]
      ; duration : string [@default ""]
      }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson]

    type t =
      { result : result list [@default []]
      ; (* present when risk control swaps real results for a captcha challenge *)
        v_voucher : string option [@default None]
      }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson]
  end
end

module Search_result = struct
  type t =
    { bvid : string
    ; title : string
    ; author : string option
    ; duration : string option
    }
  [@@deriving sexp_of, fields ~getters]

  (* Search titles embed the matched terms in [<em class="keyword">...</em>] and
     use a few HTML entities; render them as plain text. *)
  let strip_tags s =
    let buf = Buffer.create (String.length s) in
    let in_tag = ref false in
    String.iter s ~f:(fun c ->
      match c with
      | '<' -> in_tag := true
      | '>' -> in_tag := false
      | c -> if not !in_tag then Buffer.add_char buf c);
    Buffer.contents buf
  ;;

  let unescape s =
    List.fold
      [ "&amp;", "&"; "&lt;", "<"; "&gt;", ">"; "&quot;", "\""; "&#39;", "'" ]
      ~init:s
      ~f:(fun s (pattern, with_) -> String.substr_replace_all s ~pattern ~with_)
  ;;

  let optional s =
    match String.strip s with
    | "" -> None
    | s -> Some s
  ;;

  let of_api { Api.Search.bvid; title; author; duration } =
    { bvid
    ; title = title |> strip_tags |> unescape |> String.strip
    ; author = optional author
    ; duration = optional duration
    }
  ;;
end

(* Resolve the [cid] (the id bilibili uses to identify a specific playable part)
   for the requested [part], defaulting to the first part. *)
let get_cid ~bvid ~part =
  let url =
    Uri.of_string
      [%string "https://api.bilibili.com/x/web-interface/view?bvid=%{bvid}"]
  in
  let%map.Deferred.Or_error view = get_json ~url ~of_yojson:Api.View.t_of_yojson () in
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
    get_json ~url ~of_yojson:Api.Playurl.t_of_yojson ()
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

(* The search endpoint needs an anonymous session: a [buvid3] cookie (and ideally
   a few friends) set the way a browser would get them. Best-effort — on any
   failure we fall back to whatever cookies (if any) we managed to collect. *)
let interesting_cookies = [ "buvid3"; "buvid4"; "b_nut" ]

let set_cookies_of_response response =
  Cohttp.Response.headers response
  |> Fn.flip Cohttp.Header.get_multi "set-cookie"
  |> List.filter_map ~f:(fun set_cookie ->
    let pair = String.lsplit2 set_cookie ~on:';' |> Option.value_map ~f:fst ~default:set_cookie in
    match String.lsplit2 pair ~on:'=' with
    | Some (name, value) when List.mem interesting_cookies (String.strip name) ~equal:String.equal
      -> Some (String.strip name, String.strip value)
    | _ -> None)
;;

let cookie_header pairs =
  List.map pairs ~f:(fun (name, value) -> [%string "%{name}=%{value}"])
  |> String.concat ~sep:"; "
;;

let with_cookies pairs headers =
  match pairs with
  | [] -> headers
  | pairs -> Cohttp.Header.add headers "cookie" (cookie_header pairs)
;;

(* Visit the homepage (and fall back to the spi fingerprint endpoint) to obtain
   browser-like cookies, returning the collected [name, value] pairs (possibly
   empty). Best-effort. *)
let prime_cookies () =
  let%bind from_homepage =
    match%map
      Monitor.try_with (fun () ->
        let%bind response, body =
          Cohttp_async.Client.get ~headers (Uri.of_string "https://www.bilibili.com/")
        in
        let%map (_ : string) = Cohttp_async.Body.to_string body in
        set_cookies_of_response response)
    with
    | Ok pairs -> pairs
    | Error _ -> []
  in
  if List.Assoc.mem from_homepage "buvid3" ~equal:String.equal
  then return from_homepage
  else (
    match%map get_json ~url:(Uri.of_string "https://api.bilibili.com/x/frontend/finger/spi") ~of_yojson:Api.Spi.t_of_yojson () with
    | Ok { b_3 } -> ("buvid3", b_3) :: from_homepage
    | Error _ -> from_homepage)
;;

let hmac_sha256_hex ~key message =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key) message
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
;;

(* [bili_ticket] is a short-lived signed token the web client sends as a cookie;
   the docs describe it as lowering anti-bot risk control. The [GenWebTicket]
   endpoint mints one given an HMAC-SHA256 signature over the current timestamp.
   Best-effort: on any failure we simply search without it.
   See https://github.com/SocialSisterYi/bilibili-API-collect (bili_ticket). *)
let get_bili_ticket ~headers =
  match%map
    Monitor.try_with (fun () ->
      let ts =
        Time_ns.now () |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_int_sec
      in
      let hexsign = hmac_sha256_hex ~key:"XgwSnGZ1p" [%string "ts%{ts#Int}"] in
      let url =
        Uri.add_query_params'
          (Uri.of_string
             "https://api.bilibili.com/bapis/bilibili.api.ticket.v1.Ticket/GenWebTicket")
          [ "key_id", "ec02"
          ; "hexsign", hexsign
          ; "context[ts]", Int.to_string ts
          ; "csrf", ""
          ]
      in
      let%bind _response, body = Cohttp_async.Client.post ~headers url in
      let%map body = Cohttp_async.Body.to_string body in
      let open Yojson.Safe.Util in
      Yojson.Safe.from_string body |> member "data" |> member "ticket" |> to_string)
  with
  | Ok ticket when not (String.is_empty ticket) -> Some ticket
  | Ok _ | Error _ -> None
;;

(* "Activate" the buvid so risk control trusts it. Fire-and-forget. *)
let activate_buvid ~headers =
  Monitor.try_with (fun () ->
    let headers = Cohttp.Header.add headers "content-type" "application/json" in
    let payload = `Assoc [ "3064", `Int 1 ] |> Yojson.Safe.to_string in
    let body = `String (`Assoc [ "payload", `String payload ] |> Yojson.Safe.to_string) in
    let%bind _response, body =
      Cohttp_async.Client.post
        ~headers
        ~body
        (Uri.of_string "https://api.bilibili.com/x/internal/gaia-gateway/ExClimbWuzhi")
    in
    Cohttp_async.Body.to_string body |> Deferred.ignore_m)
  |> Deferred.ignore_m
;;

(* wbi signing: recent search endpoints require a [w_rid]/[wts] signature derived
   from rotating keys served by the nav endpoint.
   See https://github.com/SocialSisterYi/bilibili-API-collect (wbi). *)
let mixin_key_enc_tab =
  [| 46; 47; 18; 2; 53; 8; 23; 32; 15; 50; 10; 31; 58; 3; 45; 35; 27; 43; 5; 49; 33; 9
   ; 42; 19; 29; 28; 14; 39; 12; 38; 41; 13; 37; 48; 7; 16; 24; 55; 40; 61; 26; 17; 0; 1
   ; 60; 51; 30; 4; 22; 25; 54; 21; 56; 59; 6; 63; 57; 62; 11; 36; 20; 34; 44; 52
  |]
;;

(* "https://i0.hdslb.com/bfs/wbi/<key>.png" -> "<key>" *)
let key_of_wbi_url url =
  Uri.of_string url |> Uri.path |> Filename.basename |> Filename.split_extension |> fst
;;

let get_wbi_mixin_key ~headers =
  let url = Uri.of_string "https://api.bilibili.com/x/web-interface/nav" in
  let%bind _response, body = Cohttp_async.Client.get ~headers url in
  let%map body = Cohttp_async.Body.to_string body in
  (* nav returns code -101 when anonymous, but still includes the wbi keys. *)
  Or_error.try_with (fun () ->
    let open Yojson.Safe.Util in
    let wbi = Yojson.Safe.from_string body |> member "data" |> member "wbi_img" in
    let raw =
      key_of_wbi_url (wbi |> member "img_url" |> to_string)
      ^ key_of_wbi_url (wbi |> member "sub_url" |> to_string)
    in
    String.init 32 ~f:(fun i -> raw.[mixin_key_enc_tab.(i)]))
;;

(* Match JavaScript's encodeURIComponent-via-URLSearchParams: keep [A-Za-z0-9_.-~],
   space becomes '+', everything else percent-encoded uppercase. *)
let urlencode_plus s =
  String.concat_map s ~f:(fun c ->
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '-' | '~' -> String.of_char c
    | ' ' -> "+"
    | c -> sprintf "%%%02X" (Char.to_int c))
;;

(* Pure core: given the params (including [wts]) and the mixin key, append the
   [w_rid] signature. Sorted, plus-encoded, md5'd with the key appended. *)
let add_w_rid params ~mixin_key =
  let query =
    List.sort params ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (k, v) -> [%string "%{k}=%{urlencode_plus v}"])
    |> String.concat ~sep:"&"
  in
  let w_rid = Stdlib.Digest.string (query ^ mixin_key) |> Stdlib.Digest.to_hex in
  params @ [ "w_rid", w_rid ]
;;

let sign_params params ~mixin_key =
  let wts =
    Time_ns.now () |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_int_sec |> Int.to_string
  in
  add_w_rid (params @ [ "wts", wts ]) ~mixin_key
;;

let search ?sessdata ~max_results query =
  let%bind cookies = prime_cookies () in
  (* A logged-in [SESSDATA] cookie lifts the request out of the anonymous
     risk-control tier — the only thing that reliably clears search from a
     datacenter IP. *)
  let cookies =
    match sessdata with
    | Some sessdata -> ("SESSDATA", sessdata) :: cookies
    | None -> cookies
  in
  let%bind ticket = get_bili_ticket ~headers:(with_cookies cookies headers) in
  let cookies =
    match ticket with
    | Some ticket -> cookies @ [ "bili_ticket", ticket ]
    | None -> cookies
  in
  let headers = with_cookies cookies headers in
  let%bind () = activate_buvid ~headers in
  let%bind.Deferred.Or_error mixin_key = get_wbi_mixin_key ~headers in
  let params =
    sign_params
      [ "search_type", "video"; "keyword", query; "page", "1" ]
      ~mixin_key
  in
  let url =
    Uri.add_query_params'
      (Uri.of_string "https://api.bilibili.com/x/web-interface/wbi/search/type")
      params
  in
  let%bind.Deferred.Or_error { Api.Search.result; v_voucher } =
    get_json ~headers ~url ~of_yojson:Api.Search.t_of_yojson ()
  in
  match v_voucher with
  | Some _ ->
    Deferred.Or_error.error_s
      [%message
        "Bilibili blocked the search with anti-bot risk control (no results \
         returned). This is typically an IP-reputation problem on \
         server/datacenter networks; searching from a residential network, or \
         supplying a logged-in cookie, avoids it."
          (query : string)]
  | None ->
    List.take result max_results |> List.map ~f:Search_result.of_api |> Deferred.Or_error.return
;;

(* [video] is the BV id; [part] is the optional 1-based part number. *)
let download ~video:bvid ~part =
  let%bind.Deferred.Or_error cid = get_cid ~bvid ~part in
  let%bind.Deferred.Or_error audio_urls = get_audio_urls ~bvid ~cid in
  download_audio audio_urls
;;

module%test _ = struct
  let%expect_test "search result title cleanup" =
    let test api = Search_result.of_api api |> [%sexp_of: Search_result.t] |> print_s in
    test
      { bvid = "BV1ez421X7Gz"
      ; title = {|<em class="keyword">love</em> 2000 &amp; more|}
      ; author = "Soraya"
      ; duration = "3:42"
      };
    test { bvid = "BV1S14y1T7uj"; title = "plain title"; author = ""; duration = "" };
    [%expect
      {|
      ((bvid BV1ez421X7Gz) (title "love 2000 & more") (author (Soraya))
       (duration (3:42)))
      ((bvid BV1S14y1T7uj) (title "plain title") (author ()) (duration ()))
      |}];
    return ()
  ;;

  (* Cross-checked against the Python reference implementation. *)
  let%expect_test "wbi url-encoding and signature" =
    print_endline (urlencode_plus "hello world 異世界");
    [%expect {| hello+world+%E7%95%B0%E4%B8%96%E7%95%8C |}];
    let signed =
      add_w_rid
        [ "keyword", "hello world 異世界"
        ; "page", "1"
        ; "search_type", "video"
        ; "wts", "1700000000"
        ]
        ~mixin_key:"1234567890abcdef1234567890abcdef"
    in
    print_s [%sexp (List.Assoc.find_exn signed "w_rid" ~equal:String.equal : string)];
    [%expect {| 1dbf2d3a71a9ce9e8baced43d2a2050f |}];
    return ()
  ;;

  (* The bili_ticket [hexsign]; cross-checked against Python's hmac. *)
  let%expect_test "bili_ticket hexsign" =
    print_endline (hmac_sha256_hex ~key:"XgwSnGZ1p" "ts1700000000");
    [%expect {| bb79f0d980ffbb51597aa1a3e8b55603025cc1322ac766f4c1a98852e6182514 |}];
    return ()
  ;;
end
