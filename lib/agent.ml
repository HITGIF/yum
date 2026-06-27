open! Core
open! Async

let user_agent = "Yum (https://github.com/hitgif/yum, 2.0)"

(* Custom ids tying the search modal together: the modal the [Search] button
   opens, and its single text input. The server matches these when the modal is
   submitted. *)
let search_modal_custom_id = "yum_search_modal"
let search_query_input_custom_id = "query"

module Emoji = struct
  type t =
    | Yum
    | Fearful
    | Pleading_face
    | Thinking
    | Arrow_forward
    | Arrow_up
    | Arrow_double_up
    | Fast_forward
    | Repeat
    | Stop_button
    | Wave
    | Mag
    | Clipboard
    | Regional_indicator_y
    | Regional_indicator_b
  [@@deriving sexp_of, to_string ~capitalize:"snake_case"]

  let to_name = to_string
  let to_string t = [%string ":%{to_name t}:"]

  let to_unicode = function
    | Yum -> "😋"
    | Fearful -> "😨"
    | Pleading_face -> "🥺"
    | Thinking -> "🤔"
    | Arrow_forward -> "▶️"
    | Arrow_up -> "⬆️"
    | Arrow_double_up -> "⏫"
    | Fast_forward -> "⏩"
    | Repeat -> "🔁"
    | Stop_button -> "⏹️"
    | Wave -> "👋"
    | Mag -> "🔍"
    | Clipboard -> "📋"
    | Regional_indicator_y -> "🇾"
    | Regional_indicator_b -> "🇧"
  ;;
end

module Custom_emoji = struct
  (* A Discord custom (server/application) emoji, written in chat as "<:name:id>",
     or "<a:name:id>" when animated. *)
  type t =
    { name : string
    ; id : string
    ; animated : bool
    }
  [@@deriving sexp_of]

  let of_string raw =
    let s = String.strip raw in
    let s = String.chop_prefix_if_exists s ~prefix:"<" in
    let s = String.chop_suffix_if_exists s ~suffix:">" in
    let animated, body =
      match String.chop_prefix s ~prefix:"a:" with
      | Some body -> true, body
      | None -> false, String.chop_prefix_if_exists s ~prefix:":"
    in
    match String.lsplit2 body ~on:':' with
    | Some (name, id)
      when (not (String.is_empty name))
           && (not (String.is_empty id))
           && String.for_all id ~f:Char.is_digit -> Ok { name; id; animated }
    | _ ->
      Or_error.error_string
        [%string
          "Invalid custom emoji %{raw}: expected the Discord form <:name:id> (or \
           <a:name:id> for animated)"]
  ;;
end

module Action = struct
  type t =
    | Skip
    | Stop
    | Start
    | Play of Song.t
    | Play_now of Song.t
    | Search
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
    ; emoji : Emoji.t option
    }
  [@@deriving sexp_of]
end

module Select = struct
  module Option = struct
    type t =
      { label : string
      ; description : string option
      ; emoji : [ `Unicode of Emoji.t | `Custom of Custom_emoji.t ] option
      ; action : Action.t
      }
    [@@deriving sexp_of]
  end
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
    let button { Button.style; action; label; emoji } =
      let emoji =
        let%map.Option emoji in
        { Partial_emoji.name = Emoji.to_unicode emoji; id = None; animated = None }
      in
      Button
        { style = Button.Style.to_int style
        ; custom_id = Action.to_custom_id action
        ; label
        ; emoji
        }
    in
    (* Discord allows at most 5 buttons per action row, so spread them across
       rows of 5. *)
    let action_rows =
      List.chunks_of buttons ~length:5
      |> List.map ~f:(fun buttons ->
        Action_row { components = List.map buttons ~f:button })
    in
    send_components_message t (text_display @ action_rows)
;;

let send_message ?buttons ?code ?emoji ?emoji_end t message =
  send_message' ?buttons ?code ?emoji ?emoji_end t (Some message)
;;

let send_select ?emoji ?placeholder t message options =
  let open Discord.Http.Create_message.Component in
  let text_display =
    match content ?emoji (Some message) with
    | None -> []
    | Some content -> [ Text_display { content } ]
  in
  let options =
    List.map options ~f:(fun { Select.Option.label; description; emoji; action } ->
      let emoji =
        Option.map emoji ~f:(function
          | `Unicode emoji ->
            { Partial_emoji.name = Emoji.to_unicode emoji; id = None; animated = None }
          | `Custom { Custom_emoji.name; id; animated } ->
            { Partial_emoji.name; id = Some id; animated = Some animated })
      in
      { Select_option.label; value = Action.to_custom_id action; description; emoji })
  in
  send_components_message
    t
    (text_display
     @ [ Action_row
           { components =
               [ String_select { custom_id = "yum_search_select"; options; placeholder } ]
           }
       ])
;;

let reset_select t ~message_id ~components =
  (* Re-send the message's own components verbatim. A string select keeps the
     last-picked option highlighted on the client, and re-picking it fires no
     interaction; editing the message back to these (unselected) components
     clears that so the same option can be picked again. *)
  Discord.Http.Edit_message.call
    ~auth_token:t.auth_token
    ~user_agent
    ~channel_id:t.channel_id
    ~message_id
    ~flags:[ Is_components_v2 ]
    ~components
  |> Deferred.ignore_m
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

let show_search_modal t ~interaction_id ~interaction_token =
  let open Discord.Http.Create_message.Component in
  Discord.Http.Show_modal.call
    ~auth_token:t.auth_token
    ~user_agent
    ~interaction_id
    ~interaction_token
    ~custom_id:search_modal_custom_id
    ~title:"Search"
    ~components:
      [ Action_row
          { components =
              [ Text_input
                  { custom_id = search_query_input_custom_id
                  ; style = 1 (* short, single-line *)
                  ; label = "What do you want to play?"
                  ; placeholder = Some "song name, artist, keywords…"
                  ; required = Some true
                  }
              ]
          }
      ]
  |> Deferred.ignore_m
;;

let register_slash_commands ~auth_token ~application_id commands =
  Discord.Http.Bulk_overwrite_commands.call
    ~auth_token
    ~user_agent
    ~application_id
    commands
  |> Deferred.ignore_m
;;

module%test _ = struct
  let%expect_test "Custom_emoji.of_string" =
    let test s = Custom_emoji.of_string s |> [%sexp_of: Custom_emoji.t Or_error.t] |> print_s in
    test "<:youtube:123456789>";
    test "<a:loading:987654321>";
    test "bilibili:42";
    test "🇾";
    [%expect
      {|
      (Ok ((name youtube) (id 123456789) (animated false)))
      (Ok ((name loading) (id 987654321) (animated true)))
      (Ok ((name bilibili) (id 42) (animated false)))
      (Error
       "Invalid custom emoji \240\159\135\190: expected the Discord form <:name:id> (or <a:name:id> for animated)")
      |}];
    return ()
  ;;
end
