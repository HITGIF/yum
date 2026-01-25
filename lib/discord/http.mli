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

module Create_message : sig
  module Flag : sig
    type t = IS_COMPONENTS_V2 [@@deriving sexp_of]
  end

  module Component : sig
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
    [@@deriving sexp_of]
  end

  module Request : sig
    type t =
      { content : string option
      ; flags : Flag.t option
      ; components : Component.t list option
      }
    [@@deriving sexp_of]
  end

  val call
    :  auth_token:Model.Auth_token.t
    -> channel_id:Model.Channel_id.t
    -> Request.t
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
    type t = { content : string option } [@@deriving sexp_of]
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
    -> interation_id:Model.Interaction_id.t
    -> interaction_token:Model.Interaction_token.t
    -> Request.t
    -> Json.t Response.t Deferred.t
end
