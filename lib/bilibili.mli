open! Core
open! Async

(** [download ~video ~part] streams the highest-bandwidth audio for the given
    bilibili [video] (a BV id) and optional 1-based [part]. *)
val download : video:string -> part:int option -> Reader.t Deferred.Or_error.t

(** [get_title ~bvid] resolves a bilibili video's title via the view API. *)
val get_title : ?sessdata:string -> bvid:string -> unit -> string Deferred.Or_error.t

module Search_result : sig
  type t =
    { bvid : string
    ; title : string
    ; author : string option
    ; duration : string option
    }
  [@@deriving sexp_of]
end

(** [search ~max_results query] runs a bilibili video keyword search, returning
    up to [max_results] results. [sessdata] is a logged-in account's [SESSDATA]
    cookie; supplying it is generally required for the search endpoint to return
    results from a datacenter/VPS IP. *)
val search
  :  ?sessdata:string
  -> max_results:int
  -> string
  -> Search_result.t list Deferred.Or_error.t
