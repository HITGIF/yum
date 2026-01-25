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

type t =
  { auth_token : Model.Auth_token.t
  ; channel_id : Model.Channel_id.t
  }

let create ~auth_token ~channel_id = { auth_token; channel_id }

let send_message' t content =
  Http.Create_message.call
    ~auth_token:t.auth_token
    ~channel_id:t.channel_id
    { content = Some content }
  |> Deferred.ignore_m
;;

let send_message ?code ?emoji ?emoji_end t message =
  let emoji = Option.map emoji ~f:Emoji.to_string in
  let emoji_end = Option.map emoji_end ~f:Emoji.to_string in
  let message =
    match code with
    | Some () -> [%string "```%{message}```"]
    | None -> message
  in
  let content =
    [ emoji; Some message; emoji_end ] |> List.filter_opt |> String.concat ~sep:" "
  in
  send_message' t content
;;
