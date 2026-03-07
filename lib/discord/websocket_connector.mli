open! Core
open! Async

val connect
  :  ?time_source:Time_source.t
  -> Uri.t
  -> (Cohttp.Response.t * Websocket.Frame_content.t Websocket.t) Deferred.Or_error.t
