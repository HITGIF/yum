open! Core
open! Async
open! Common

module Response : sig
  type 'body t =
    { status_code : Cohttp.Code.status_code
    ; body : 'body option
    }
  [@@deriving sexp_of]
end

module Flags : sig
  type flag =
    | Ephemeral
    | Is_components_v2
  [@@deriving sexp_of]

  type t = flag list
end

module Create_message : sig
  module Component : sig
    module Partial_emoji : sig
      type t =
        { name : string
        ; id : string option
        ; animated : bool option
        }
    end

    module Select_option : sig
      type t =
        { label : string
        ; value : string
        ; description : string option [@default None]
        ; emoji : Partial_emoji.t option [@default None]
        }
    end

    type t =
      | Action_row of { components : t list }
      | Button of
          { style : int
          ; custom_id : string
          ; label : string option [@default None]
          ; emoji : Partial_emoji.t option [@default None]
          }
      | String_select of
          { custom_id : string
          ; options : Select_option.t list
          ; placeholder : string option [@default None]
          }
      | Text_input of
          { custom_id : string
          ; style : int
          ; label : string
          ; placeholder : string option [@default None]
          ; required : bool option [@default None]
          }
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
    [@@deriving sexp_of]
  end

  module Request : sig
    type t =
      { content : string option
      ; flags : Flags.t option
      ; components : Component.t list option
      }
    [@@deriving sexp_of]
  end

  val call
    :  auth_token:Model.Auth_token.t
    -> user_agent:string
    -> channel_id:Model.Channel_id.t
    -> Request.t
    -> Json.t Response.t Deferred.t
end

module Edit_message : sig
  (** Edits a message. [components] are sent verbatim as raw JSON (not re-typed
      through {!Create_message.Component}), so the caller can echo a message's
      own components back to reset a select menu. *)
  val call
    :  auth_token:Model.Auth_token.t
    -> user_agent:string
    -> channel_id:Model.Channel_id.t
    -> message_id:Model.Message_id.t
    -> flags:Flags.t
    -> components:Json.t list
    -> Json.t Response.t Deferred.t
end

module Respond_interaction : sig
  module Type : sig
    type t =
      | Pong
      | Channel_message_with_source
    [@@deriving sexp_of]
  end

  module Data : sig
    type t =
      { content : string option
      ; flags : Flags.t option
      }
    [@@deriving sexp_of]
  end

  module Request : sig
    type t =
      { type_ : Type.t option
      ; data : Data.t option
      }
    [@@deriving sexp_of]
  end

  val call
    :  auth_token:Model.Auth_token.t
    -> user_agent:string
    -> interation_id:Model.Interaction_id.t
    -> interaction_token:Model.Interaction_token.t
    -> Request.t
    -> Json.t Response.t Deferred.t
end

module Show_modal : sig
  (** Responds to a component interaction by opening a modal. [components] are
      action rows of text inputs. *)
  val call
    :  auth_token:Model.Auth_token.t
    -> user_agent:string
    -> interaction_id:Model.Interaction_id.t
    -> interaction_token:Model.Interaction_token.t
    -> custom_id:string
    -> title:string
    -> components:Create_message.Component.t list
    -> Json.t Response.t Deferred.t
end

module Bulk_overwrite_commands : sig
  val call
    :  auth_token:Model.Auth_token.t
    -> user_agent:string
    -> application_id:Model.User_id.t
    -> Model.Slash_command.t list
    -> Json.t Response.t Deferred.t
end
