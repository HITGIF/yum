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
  module Request : sig
    type t = { content : string option } [@@deriving sexp_of]
  end

  val call
    :  auth_token:Model.Auth_token.t
    -> channel_id:Model.Channel_id.t
    -> Request.t
    -> Json.t Response.t Deferred.t
end
