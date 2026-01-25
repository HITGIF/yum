open! Core
open! Async

module Emoji = struct
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
  [@@deriving sexp_of, to_string ~capitalize:"snake_case"]

  let to_string t = [%string ":%{to_string t}:"]
end

module Button = struct
  module Style = struct
    type t =
      | Primary
      | Secondary
      | Success
      | Danger
    [@@deriving sexp_of]

    let to_int t =
      match t with
      | Primary -> 1
      | Secondary -> 2
      | Success -> 3
      | Danger -> 4
    ;;
  end

  type t =
    { style : Style.t
    ; custom_id : string
    ; label : string option
    }
  [@@deriving sexp_of]
end

type t =
  { auth_token : Model.Auth_token.t
  ; channel_id : Model.Channel_id.t
  }

let create ~auth_token ~channel_id = { auth_token; channel_id }

let create_massage t request =
  Http.Create_message.call ~auth_token:t.auth_token ~channel_id:t.channel_id request
  |> Deferred.ignore_m
;;

let send_normal_message t content =
  create_massage t { content; flags = None; components = None }
;;

let send_components_message t components =
  create_massage
    t
    { content = None; flags = Some IS_COMPONENTS_V2; components = Some components }
;;

let content ?code ?emoji ?emoji_end message =
  let emoji = Option.map emoji ~f:Emoji.to_string in
  let emoji_end = Option.map emoji_end ~f:Emoji.to_string in
  let message =
    match message, code with
    | None, _ -> None
    | Some message, Some () -> Some [%string "```%{message}```"]
    | Some message, None -> Some message
  in
  match [ emoji; message; emoji_end ] |> List.filter_opt with
  | [] -> None
  | components -> Some (String.concat components ~sep:" ")
;;

let send_message' ?buttons ?code ?emoji ?emoji_end t message =
  let content = content ?code ?emoji ?emoji_end message in
  match buttons with
  | None -> send_normal_message t content
  | Some buttons ->
    let open Http.Create_message.Component in
    let text_display =
      match content with
      | None -> []
      | Some content -> [ Text_display { content } ]
    in
    send_components_message
      t
      (text_display
       @ [ Action_row
             { components =
                 List.map buttons ~f:(fun { Button.style; custom_id; label } ->
                   Button
                     { style = Button.Style.to_int style; custom_id; label; emoji = None })
             }
         ])
;;

let send_message ?buttons ?code ?emoji ?emoji_end t message =
  send_message' ?buttons ?code ?emoji ?emoji_end t (Some message)
;;
