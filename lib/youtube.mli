open! Core
open! Async

val default_prog : File_path.Absolute.t

val download
  :  ?cancellation_token:unit Deferred.t
  -> ?on_finish:((unit, string) result -> unit Deferred.t)
  -> ?prog:File_path.Absolute.t
  -> ?args:string list
  -> string
  -> Reader.t Deferred.Or_error.t

val get_playlist
  :  ?prog:File_path.Absolute.t
  -> ?args:string list
  -> string
  -> Song.t list Deferred.Or_error.t

module Search_result : sig
  type t =
    { id : string
    ; title : string
    ; uploader : string option
    ; duration : string option
    }
  [@@deriving sexp_of]

  val of_line : string -> t option
end

(** [search ~max_results query] runs a YouTube keyword search via yt-dlp's
    [ytsearch], returning up to [max_results] results. *)
val search
  :  ?prog:File_path.Absolute.t
  -> max_results:int
  -> string
  -> Search_result.t list Deferred.Or_error.t
