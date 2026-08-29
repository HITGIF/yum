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

(** [search ~youtube ~query ()] runs a keyword search across both YouTube and Bilibili,
    taking the top 15 hits from each and merging them into a single list ordered by our
    own relevance score (so neither platform can crowd the other out of the menu, and
    neither can take more than 15 of the slots). Results are already trimmed to Discord's
    per-field length limits and capped at Discord's 25-option select-menu limit, which
    trims the lowest-scoring 5 of the 30 candidates. If one source errors, the other's
    results are still returned; only when both fail is an error surfaced.

    [bilibili_sessdata] is a logged-in [SESSDATA] cookie used for the Bilibili half
    (generally required to clear search risk control); it has no effect on the YouTube
    half. *)
val search
  :  ?bilibili_sessdata:string
  -> youtube:Youtube.t
  -> query:string
  -> unit
  -> Result.t list Deferred.Or_error.t
