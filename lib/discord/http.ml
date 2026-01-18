open! Core
open! Async
open! Common
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

let headers ~auth_token =
  [ "authorization", "Bot " ^ Model.Auth_token.to_string auth_token
  ; "user-agent", "Yum (https://github.com/hitgif/yum, 2.0)"
  ; "content-Type", "application/json"
  ; "accept", "*/*"
  ]
  |> Cohttp.Header.of_list
;;

let path subpath = [ "api"; "v10" ] @ subpath |> String.concat ~sep:"/"

let url subpath =
  let f = Fn.flip in
  Uri.empty
  |> f Uri.with_scheme (Some "https")
  |> f Uri.with_host (Some "discord.com")
  |> f Uri.with_path (path subpath)
;;

module Response = struct
  type 'body t =
    { status_code : Cohttp.Code.status_code
    ; body : 'body option
    }
  [@@deriving sexp_of]
end

module type Response_body = sig
  type t [@@deriving sexp_of]

  val t_of_yojson : Yojson.Safe.t -> t
end

let call
  (type response_body)
  (module Response_body : Response_body with type t = response_body)
  ?body
  ~auth_token
  method_
  subpath
  =
  let url = url subpath in
  [%log.debug
    [%here]
      "HTTP request"
      ~_:(method_ : Cohttp.Code.meth)
      ~_:(Model.Uri.of_uri url : Model.Uri.t)
      ~_:(body : Json.t option)];
  let body =
    let%map.Option body in
    `String (Json.to_string body)
  in
  let%bind response, body =
    Cohttp_async.Client.call ~headers:(headers ~auth_token) ?body method_ url
  in
  let status_code = Cohttp.Response.status response in
  let%bind body = Cohttp_async.Body.to_string body in
  let body =
    Or_error.try_with (fun () -> Json.from_string body |> [%of_yojson: Response_body.t])
    |> Or_error.ok
  in
  let response = { Response.status_code; body } in
  [%log.debug
    [%here]
      "HTTP response"
      ~_:(method_ : Cohttp.Code.meth)
      ~_:(Model.Uri.of_uri url : Model.Uri.t)
      ~_:(response : Response_body.t Response.t)];
  return response
;;

module Create_message = struct
  module Request = struct
    type t = { content : string option [@default None] }
    [@@yojson.allow_extra_fields] [@@deriving sexp_of, yojson]
  end

  let call ~auth_token ~channel_id request =
    call
      (module Json)
      `POST
      ~body:([%yojson_of: Request.t] request)
      ~auth_token
      [ "channels"; Model.Channel_id.to_string channel_id; "messages" ]
  ;;
end

module%test _ = struct
  let%expect_test "headers" =
    print_s
      [%sexp
        (headers ~auth_token:(Model.Auth_token.of_string "test_token") : Cohttp.Header.t)];
    [%expect
      {|
    ((authorization "Bot test_token")
     (user-agent "Yum (https://github.com/hitgif/yum, 2.0)")
     (content-Type application/json) (accept */*))
    |}];
    return ()
  ;;

  let%expect_test "url" =
    print_s
      [%sexp (url [ "channels"; "0"; "messages" ] |> Model.Uri.of_uri : Model.Uri.t)];
    [%expect {| https://discord.com/api/v10/channels/0/messages |}];
    return ()
  ;;
end
