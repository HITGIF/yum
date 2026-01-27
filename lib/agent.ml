open! Core
open! Async

let user_agent = "Yum (https://github.com/hitgif/yum, 2.0)"

module Emoji = struct
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
    | Arrow_forward
    | Arrow_double_up
    | Fast_forward
    | Repeat
  [@@deriving sexp_of, to_string ~capitalize:"snake_case"]

  let to_string t = [%string ":%{to_string t}:"]
end

module Action = struct
  type t =
    | Skip
    | Play of Song.t
    | Play_now of Song.t
    | Unknown of string
  [@@deriving sexp]

  let to_custom_id = Fn.compose Sexp.to_string [%sexp_of: t]

  let of_custom_id id =
    match Or_error.try_with (fun () -> Sexp.of_string id |> [%of_sexp: t]) with
    | Ok action -> action
    | Error _ -> Unknown id
  ;;
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
    ; action : Action.t
    ; label : string option
    }
  [@@deriving sexp_of]
end

type t =
  { auth_token : Discord.Model.Auth_token.t
  ; channel_id : Discord.Model.Channel_id.t
  }

let create ~auth_token ~channel_id = { auth_token; channel_id }

let create_massage t request =
  Discord.Http.Create_message.call
    ~auth_token:t.auth_token
    ~user_agent
    ~channel_id:t.channel_id
    request
  |> Deferred.ignore_m
;;

let send_normal_message t content =
  create_massage t { content; flags = None; components = None }
;;

let send_components_message t components =
  create_massage
    t
    { content = None; flags = Some [ Is_components_v2 ]; components = Some components }
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
    let open Discord.Http.Create_message.Component in
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
                 List.map buttons ~f:(fun { Button.style; action; label } ->
                   Button
                     { style = Button.Style.to_int style
                     ; custom_id = Action.to_custom_id action
                     ; label
                     ; emoji = None
                     })
             }
         ])
;;

let send_message ?buttons ?code ?emoji ?emoji_end t message =
  send_message' ?buttons ?code ?emoji ?emoji_end t (Some message)
;;

let respond_interaction ?emoji ?emoji_end t id token message =
  let content = content ?emoji ?emoji_end (Some message) in
  Discord.Http.Respond_interaction.call
    ~auth_token:t.auth_token
    ~user_agent
    ~interation_id:id
    ~interaction_token:token
    { type_ = Some Channel_message_with_source
    ; data = Some { content; flags = Some [ Ephemeral ] }
    }
  |> Deferred.ignore_m
;;
