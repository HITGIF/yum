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
  module Flag = struct
    type t = IS_COMPONENTS_V2 [@@deriving sexp_of]

    let to_int = function
      | IS_COMPONENTS_V2 -> 1 lsl 15
    ;;

    let yojson_of_t t = t |> to_int |> [%yojson_of: int]
  end

  module Component = struct
    type t =
      | Action_row of { components : t list }
      | Button of
          { style : int
          ; custom_id : string
          ; label : string option [@default None]
          ; emoji : string option [@default None]
          }
      | String_select
      | Text_input
      | User_select
      | Role_select
      | Mentionable_select
      | Channel_select
      | Section
      | Text_display of { content : string }
      | Thumbnail
      | Media_gallery
      | File
      | Separator
      | Unused_15
      | Unused_16
      | Container
      | Label
      | File_upload
    [@@deriving sexp_of, yojson_of, variants]

    let type_of_name =
      let f acc { Variant.name; rank; _ } = Map.set acc ~key:name ~data:(rank + 1) in
      Variants.fold
        ~init:String.Map.empty
        ~action_row:f
        ~button:f
        ~string_select:f
        ~text_input:f
        ~user_select:f
        ~role_select:f
        ~mentionable_select:f
        ~channel_select:f
        ~section:f
        ~text_display:f
        ~thumbnail:f
        ~media_gallery:f
        ~file:f
        ~separator:f
        ~unused_15:f
        ~unused_16:f
        ~container:f
        ~label:f
        ~file_upload:f
    ;;

    let rec typed_yojson_of = function
      | `List [ `String name; `Assoc fields ] ->
        let fields = List.map fields ~f:(Tuple2.map_snd ~f:typed_yojson_of) in
        (match Map.find type_of_name name with
         | None -> `Assoc fields
         | Some type_ -> `Assoc (("type", [%yojson_of: int] type_) :: fields))
      | `List jsons -> `List (List.map jsons ~f:typed_yojson_of)
      | `Tuple jsons -> `Tuple (List.map jsons ~f:typed_yojson_of)
      | `Variant (name, json) -> `Variant (name, Option.map json ~f:typed_yojson_of)
      | `Assoc fields -> `Assoc (List.map fields ~f:(Tuple2.map_snd ~f:typed_yojson_of))
      | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as atom -> atom
    ;;

    let yojson_of_t t = [%yojson_of: t] t |> typed_yojson_of
  end

  module Request = struct
    type t =
      { content : string option [@default None]
      ; flags : Flag.t option [@default None]
      ; components : Component.t list option [@default None]
      }
    [@@deriving sexp_of, yojson_of]
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

  let%expect_test "message components" =
    let open Create_message.Component in
    let test ts = [%yojson_of: t list] ts |> Json.pretty_to_string |> print_endline in
    test
      [ Action_row
          { components =
              [ Button
                  { style = 1
                  ; custom_id = "click_me_1"
                  ; label = Some "Click Me"
                  ; emoji = None
                  }
              ; Button
                  { style = 2
                  ; custom_id = "click_me_2"
                  ; label = Some "Click Me Too"
                  ; emoji = None
                  }
              ]
          }
      ];
    [%expect
      {|
      [
        {
          "type": 1,
          "components": [
            {
              "type": 2,
              "style": 1,
              "custom_id": "click_me_1",
              "label": "Click Me",
              "emoji": null
            },
            {
              "type": 2,
              "style": 2,
              "custom_id": "click_me_2",
              "label": "Click Me Too",
              "emoji": null
            }
          ]
        }
      ]
      |}];
    test [ Button { style = 1; custom_id = "1"; label = None; emoji = None } ];
    [%expect
      {| [ { "type": 2, "style": 1, "custom_id": "1", "label": null, "emoji": null } ] |}];
    return ()
  ;;
end
