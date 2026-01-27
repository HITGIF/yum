open! Core
open! Async
open! Common

open struct
  open Discord.Model
  module Auth_token = Auth_token
  module Channel_id = Channel_id
  module Guild_id = Guild_id
end

module Inf_random_sequence : sig
  type 'a t

  val create : 'a Nonempty_list.t -> 'a t
  val next : 'a t -> 'a
end = struct
  type 'a t =
    { elements : 'a Nonempty_list.t
    ; queue : 'a Queue.t
    }

  let enqueue t =
    Nonempty_list.to_list t.elements |> List.permute |> Queue.enqueue_all t.queue
  ;;

  let create elements =
    let t = { elements; queue = Queue.create () } in
    enqueue t;
    t
  ;;

  let rec next t =
    match Queue.dequeue t.queue with
    | Some element -> element
    | None ->
      enqueue t;
      next t
  ;;
end

module Songs = struct
  type t =
    { requested : Song.t Deque.t
    ; default : Song.t Inf_random_sequence.t
    }

  let create ~default_songs =
    { requested = Deque.create (); default = Inf_random_sequence.create default_songs }
  ;;

  let enqueue_front t = Deque.enqueue_front t.requested
  let enqueue_back t = Deque.enqueue_back t.requested

  let next t =
    match Deque.dequeue_front t.requested with
    | Some song -> song
    | None -> Inf_random_sequence.next t.default
  ;;
end

type t =
  { ffmpeg_path : File_path.Absolute.t
  ; yt_dlp_path : File_path.Absolute.t
  ; guild_id : Guild_id.t
  ; mutable agent : Agent.t
  ; mutable voice_channel : Channel_id.t
  ; mutable frames_writer : Audio.Pcm_frame.t Queue.t Pipe.Writer.t option
  ; mutable playing : Song.t option
  ; songs : Songs.t
  ; skip : (unit, read_write) Bvar.t
  ; on_new_frames_writer : (unit, read_write) Bvar.t
  ; started : unit Set_once.t
  ; closed : unit Set_once.t
  }

let playing t = t.playing

let close
  { ffmpeg_path = _
  ; yt_dlp_path = _
  ; guild_id
  ; agent = _
  ; voice_channel = _
  ; frames_writer
  ; playing = _
  ; songs = _
  ; skip = _
  ; on_new_frames_writer = _
  ; started = _
  ; closed
  }
  =
  [%log.info [%here] "Closing player" (guild_id : Guild_id.t)];
  Set_once.set_if_none closed ();
  Option.iter frames_writer ~f:Pipe.close
;;

let set_agent t agent = t.agent <- agent
let set_voice_channel t channel_id = t.voice_channel <- channel_id

let set_frames_writer t frames_writer =
  let old_frames_writer = t.frames_writer in
  t.frames_writer <- frames_writer;
  Bvar.broadcast t.on_new_frames_writer ();
  Option.iter old_frames_writer ~f:Pipe.close
;;

let write_frames t frames_reader ~cancellation_token =
  let batch_size =
    (* 60ms *)
    3
  in
  let buffer_size =
    Byte_units.(of_megabytes 64. // Audio.Pcm_frame.frame_length) |> Float.to_int
  in
  let buf_reader, buf_writer = Pipe.create ~size_budget:buffer_size () in
  let done_buffering = Pipe.transfer frames_reader buf_writer ~f:Fn.id in
  don't_wait_for
    (let%map () = Deferred.any_unit [ done_buffering; cancellation_token ] in
     Pipe.close_read frames_reader;
     Pipe.close buf_writer);
  let rec send_next_batch () =
    match%bind Pipe.read_exactly buf_reader ~num_values:batch_size with
    | `Eof -> return ()
    | `Exactly batch | `Fewer batch ->
      let rec try_write () =
        match t.frames_writer with
        | None ->
          let%bind () = Bvar.wait t.on_new_frames_writer in
          try_write ()
        | Some frames_writer ->
          if not (Pipe.is_closed frames_writer)
          then Pipe.write frames_writer batch
          else (
            let%bind () = Bvar.wait t.on_new_frames_writer in
            try_write ())
      in
      let%bind () = try_write () in
      send_next_batch ()
  in
  let done_transfering = send_next_batch () in
  let%bind () = Deferred.any_unit [ done_transfering; cancellation_token ] in
  Pipe.close_read buf_reader;
  Deferred.Or_error.ok_unit
;;

let play ~cancellation_token ({ guild_id; yt_dlp_path; ffmpeg_path; _ } as t) song =
  let%with attempt = Deferred.repeat_until_finished (Attempt.create ~max:3 ()) in
  let should_retry = Ivar.create () in
  let process_result ~tag = function
    | Ok () -> return ()
    | Error error ->
      let retryable = [ "HTTP Error 403: Forbidden" ] in
      let ignorable = [ "Broken pipe" ] in
      let is_ = List.exists ~f:(fun substring -> String.is_substring error ~substring) in
      if is_ retryable
      then (
        Ivar.fill_if_empty should_retry error;
        return ())
      else if String.is_empty error || is_ ignorable
      then return ()
      else
        Agent.send_message ~code:() ~emoji:Fearful t.agent [%string "[%{tag}] %{error}"]
  in
  let wait = ref [] in
  let with_on_finish ~tag f =
    let finish = Ivar.create () in
    wait := Ivar.read finish :: !wait;
    let on_finish result =
      let%map () = process_result ~tag result in
      Ivar.fill_exn finish ()
    in
    f on_finish
  in
  let%bind () =
    let download () =
      match Song.to_src song with
      | `Youtube url ->
        let%with on_finish = with_on_finish ~tag:"yt-dlp" in
        Yt_dlp.download ~prog:yt_dlp_path ~cancellation_token ~on_finish url
      | `Bilibili url -> Bilibili.download (Uri.of_string url)
    in
    let encode reader =
      let%with on_finish = with_on_finish ~tag:"ffmpeg" in
      Ffmpeg.encode_pcm ~prog:ffmpeg_path ~cancellation_token ~on_finish reader
    in
    match%bind download () >>=? encode >>=? write_frames t ~cancellation_token with
    | Ok () -> return ()
    | Error error ->
      let%bind () =
        let error = [%sexp_of: Error.t] error |> Sexp.to_string_hum in
        Agent.send_message ~code:() ~emoji:Fearful t.agent error
      in
      [%log.error
        [%here]
          "Error playing song"
          (guild_id : Guild_id.t)
          (song : Song.t)
          (error : Error.t)];
      return ()
  in
  let%bind () = Deferred.all_unit !wait in
  match Ivar.peek should_retry with
  | None -> return (`Finished ())
  | Some error ->
    (match Attempt.try_ attempt with
     | Ok () ->
       [%log.error [%here] "Retrying song..." (guild_id : Guild_id.t) (song : Song.t)];
       let%map () =
         Agent.send_message ~emoji:Repeat t.agent [%string "Retrying... ```%{error}```"]
       in
       `Repeat attempt
     | Error _ ->
       let%map () = Agent.send_message ~code:() ~emoji:Fearful t.agent error in
       `Finished ())
;;

let rec play_loop ({ guild_id; _ } as t) =
  Todo.pause_when_channel_empty;
  match Set_once.is_some t.closed with
  | true ->
    [%log.debug [%here] "Player closed" (guild_id : Guild_id.t)];
    return ()
  | false ->
    let cancellation_token = Bvar.wait t.skip in
    let song = Songs.next t.songs in
    [%log.info [%here] "Playing song" (guild_id : Guild_id.t) (song : Song.t)];
    let%bind () = Agent.send_message ~emoji:Arrow_forward t.agent (Song.to_url song) in
    let%bind () =
      Agent.send_message'
        ~buttons:
          [ { style = Danger; action = Skip; label = Some "Skip" }
          ; { style = Primary; action = Play song; label = Some "Play" }
          ; { style = Success; action = Play_now song; label = Some "Play!" }
          ]
        t.agent
        None
    in
    t.playing <- Some song;
    let%bind () = play ~cancellation_token t song in
    t.playing <- None;
    [%log.info [%here] "Done playing song" (guild_id : Guild_id.t) (song : Song.t)];
    play_loop t
;;

let start_once t =
  match Set_once.set t.started () with
  | Error _ -> ()
  | Ok () -> play_loop t |> don't_wait_for
;;

let started t = Set_once.is_some t.started

let queue ({ guild_id; _ } as t) song =
  [%log.info [%here] "Queueing song" (guild_id : Guild_id.t) (song : Song.t)];
  Songs.enqueue_back t.songs song
;;

let queue_all ({ guild_id; _ } as t) songs =
  [%log.info
    [%here]
      "Queueing all songs"
      (guild_id : Guild_id.t)
      ~num_songs:(List.length songs : int)];
  List.iter songs ~f:(Songs.enqueue_back t.songs)
;;

let play_now ({ guild_id; _ } as t) song =
  [%log.info [%here] "Playing song now" (guild_id : Guild_id.t) (song : Song.t)];
  Songs.enqueue_front t.songs song;
  Bvar.broadcast t.skip ()
;;

let skip { guild_id; skip; _ } =
  [%log.info [%here] "Skipping song" (guild_id : Guild_id.t)];
  Bvar.broadcast skip ()
;;

let create
  ~ffmpeg_path
  ~yt_dlp_path
  ~guild_id
  ~agent
  ~voice_channel
  ~default_songs
  ~frames_writer
  =
  { ffmpeg_path
  ; yt_dlp_path
  ; guild_id
  ; agent
  ; voice_channel
  ; frames_writer
  ; playing = None
  ; songs = Songs.create ~default_songs
  ; skip = Bvar.create ()
  ; on_new_frames_writer = Bvar.create ()
  ; started = Set_once.create ()
  ; closed = Set_once.create ()
  }
;;
