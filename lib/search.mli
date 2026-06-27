open! Core
open! Async

module Result : sig
  type t =
    { song : Song.t
    ; label : string (** option label shown in the dropdown (<= 100 chars) *)
    ; description : string (** option description, e.g. "uploader · 3:42" *)
    }
  [@@deriving sexp_of]
end

(** [search ~yt_dlp_path ~query ()] runs a keyword search across both YouTube and
    Bilibili, returning up to 25 merged results (Discord's select-menu limit), each
    already trimmed to Discord's per-field length limits and tagged with its [source]. If
    one source errors, the other's results are still returned; only when both fail is an
    error surfaced.

    [bilibili_sessdata] is a logged-in [SESSDATA] cookie used for the Bilibili half
    (generally required to clear search risk control); it has no effect on the YouTube
    half. *)
val search
  :  ?bilibili_sessdata:string
  -> yt_dlp_path:File_path.Absolute.t
  -> query:string
  -> unit
  -> Result.t list Deferred.Or_error.t
