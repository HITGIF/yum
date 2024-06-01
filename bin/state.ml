open! Core

module Guild_state = struct
  module Data = struct
    type t =
      { channel_id : string
      ; queued_videos : Video_id.t Deque.t
      }
  end

  type t =
    | Idle
    | Joining of Data.t
    | Playing of Data.t

  let init = Idle
end

type t = Guild_state.t String.Map.t

let init = String.Map.empty
