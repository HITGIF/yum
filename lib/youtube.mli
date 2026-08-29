open! Core
open! Async

type t

val default_prog : File_path.Absolute.t
val create : ?prog:File_path.Absolute.t -> ?cookies:File_path.t -> unit -> t

val download
  :  ?cancellation_token:unit Deferred.t
  -> ?on_finish:((unit, string) result -> unit Deferred.t)
  -> ?args:string list
  -> t
  -> string
  -> Reader.t Deferred.Or_error.t

val get_playlist : ?args:string list -> t -> string -> Song.t list Deferred.Or_error.t

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

(** [search ~max_results query] runs a YouTube keyword search via yt-dlp's [ytsearch],
    returning up to [max_results] results. *)
val search : t -> max_results:int -> string -> Search_result.t list Deferred.Or_error.t

(** [get_title url] resolves a single video's title. It uses YouTube's fast oEmbed
    endpoint, falling back to yt-dlp for videos oEmbed can't resolve
    (private/age-restricted/deleted). *)
val get_title : t -> string -> string Deferred.Or_error.t
