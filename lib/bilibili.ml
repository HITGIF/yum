open! Core
open! Async
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

let headers =
  Cohttp.Header.of_list
    [ ( "user-agent"
      , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like \
         Gecko) Chrome/96.0.4664.110 Safari/537.36" )
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

let decompress string =
  let gzipped = Filename_unix.temp_file "bilibili_html_gzipped" "" in
  let%bind () = Writer.save gzipped ~contents:string in
  let in_channel = Gzip.open_in gzipped in
  let buf_size = Byte_units.of_kilobytes 100. |> Byte_units.bytes_int_exn in
  let buf = Bytes.create buf_size in
  let rec read acc =
    match Gzip.input in_channel buf 0 buf_size with
    | 0 -> List.rev acc |> String.concat
    | len -> (Bytes.subo ~len buf |> Bytes.to_string) :: acc |> read
  in
  let output =
    Or_error.try_with (fun () -> read []) |> Or_error.tag ~tag:"Error decompressing HTML"
  in
  Gzip.close_in in_channel;
  Core_unix.remove gzipped;
  return output
;;

let get_html html_url =
  let%bind response, body = Cohttp_async.Client.get ~headers html_url in
  let%bind.Deferred.Or_error () =
    validate_response response
    |> Or_error.tag_s_lazy
         ~tag:[%lazy_sexp { html_url : string = Uri.to_string html_url }]
    |> return
  in
  let%bind body = Cohttp_async.Body.to_string body in
  decompress body
;;

let audio_resource_regex =
  lazy
    Re.(
      compile
        (seq
           [ str "window.__playinfo__="; group (non_greedy (rep1 any)); str "</script" ]))
;;

module Audio_resource = struct
  type audio =
    { base_url : string
    ; backup_url : string list
    ; bandwidth : int
    }
  [@@yojson.allow_extra_fields] [@@deriving sexp_of, of_yojson, fields ~getters]

  let urls { base_url; backup_url; _ } = base_url :: backup_url

  type dash = { audio : audio list }
  [@@yojson.allow_extra_fields] [@@deriving sexp_of, of_yojson]

  type data = { dash : dash }
  [@@yojson.allow_extra_fields] [@@deriving sexp_of, of_yojson]

  type t = { data : data } [@@yojson.allow_extra_fields] [@@deriving sexp_of, of_yojson]
end

let parse_audio_urls html =
  let%bind.Deferred.Or_error group =
    Or_error.try_with (fun () -> Re.exec (force audio_resource_regex) html)
    |> Or_error.tag_s_lazy
         ~tag:
           [%lazy_message
             "Error matching audio resource regex in html"
               ~html_truncated:(String.prefix html 500)]
    |> return
  in
  let%bind.Deferred.Or_error audio_resource =
    match Re.Group.get_opt group 1 with
    | None ->
      Deferred.Or_error.error_s
        [%message "Audio resource not found in HTML" (html : string)]
    | Some str ->
      Or_error.try_with (fun () ->
        Yojson.Safe.from_string str |> [%of_yojson: Audio_resource.t])
      |> Or_error.tag ~tag:"Error parsing audio resource json"
      |> return
  in
  match audio_resource.data.dash.audio with
  | [] -> Deferred.Or_error.error_s [%message "No audios found in audio resource"]
  | audios ->
    (match
       List.sort
         audios
         ~compare:
           (* highest bandwidth first *)
           (Comparable.lift
              (Comparable.reverse [%compare: int])
              ~f:Audio_resource.bandwidth)
       |> List.hd_exn
       |> Audio_resource.urls
     with
     | [] -> Deferred.Or_error.error_s [%message "No URL found for audio resource"]
     | urls -> List.map urls ~f:Uri.of_string |> Deferred.Or_error.return)
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

let download url = get_html url >>=? parse_audio_urls >>=? download_audio
