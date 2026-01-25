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
  { auth_token : Auth_token.t
  ; ffmpeg_path : File_path.Absolute.t
  ; yt_dlp_path : File_path.Absolute.t
  ; guild_id : Guild_id.t
  ; mutable voice_channel : Channel_id.t
  ; mutable message_channel : Channel_id.t
  ; mutable frames_writer : Audio.Pcm_frame.t Queue.t Pipe.Writer.t option
  ; mutable playing : Song.t option
  ; songs : Songs.t
  ; skip : (unit, read_write) Bvar.t
  ; on_song_start : (Song.t, read_write) Bvar.t
  ; on_songs_empty : (unit, read_write) Bvar.t
  ; on_new_frames_writer : (unit, read_write) Bvar.t
  ; started : unit Set_once.t
  ; closed : unit Set_once.t
  }

let playing t = t.playing
let on_song_start t = (t.on_song_start :> (Song.t, read) Bvar.t)
let on_songs_empty t = (t.on_songs_empty :> (unit, read) Bvar.t)

let close
  { auth_token = _
  ; ffmpeg_path = _
  ; yt_dlp_path = _
  ; guild_id
  ; voice_channel = _
  ; message_channel = _
  ; frames_writer
  ; playing = _
  ; songs = _
  ; skip = _
  ; on_song_start = _
  ; on_songs_empty = _
  ; on_new_frames_writer = _
  ; started = _
  ; closed
  }
  =
  [%log.info [%here] "Closing player" (guild_id : Guild_id.t)];
  Set_once.set_if_none closed ();
  Option.iter frames_writer ~f:Pipe.close
;;

let set_voice_channel t channel_id = t.voice_channel <- channel_id
let set_message_channel t channel_id = t.message_channel <- channel_id

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

let rec play ({ guild_id; _ } as t) =
  match Set_once.is_some t.closed with
  | true ->
    [%log.debug [%here] "Player closed" (guild_id : Guild_id.t)];
    return ()
  | false ->
    let cancellation_token = Bvar.wait t.skip in
    let song = Songs.next t.songs in
    [%log.info [%here] "Playing song" (guild_id : Guild_id.t) (song : Song.t)];
    let%bind () =
      let url = Song.to_url song in
      Discord.Http.Create_message.call
        ~auth_token:t.auth_token
        ~channel_id:t.message_channel
        { content = Some [%string ":yum: %{url}"] }
      |> Deferred.ignore_m
    in
    t.playing <- Some song;
    Bvar.broadcast t.on_song_start song;
    let%bind () =
      let download () =
        match Song.to_src song with
        | `Youtube url -> Yt_dlp.download ~prog:t.yt_dlp_path ~cancellation_token url
        | `Bilibili url -> Bilibili.download (Uri.of_string url)
      in
      match%bind
        download ()
        >>=? Ffmpeg.encode_pcm ~prog:t.ffmpeg_path ~cancellation_token
        >>=? write_frames t ~cancellation_token
      with
      | Ok () ->
        [%log.info [%here] "Done playing song" (guild_id : Guild_id.t) (song : Song.t)];
        return ()
      | Error error ->
        let%bind () =
          let error = [%sexp_of: Error.t] error |> Sexp.to_string_hum in
          Discord.Http.Create_message.call
            ~auth_token:t.auth_token
            ~channel_id:t.message_channel
            { content = Some [%string ":fearful: ```%{error}```"] }
          |> Deferred.ignore_m
        in
        [%log.error
          [%here]
            "Error playing song"
            (guild_id : Guild_id.t)
            (song : Song.t)
            (error : Error.t)];
        return ()
    in
    t.playing <- None;
    play t
;;

let start_once t =
  match Set_once.set t.started () with
  | Error _ -> `Already_started
  | Ok () ->
    play t |> don't_wait_for;
    `Ok
;;

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
  ~auth_token
  ~ffmpeg_path
  ~yt_dlp_path
  ~guild_id
  ~voice_channel
  ~message_channel
  ~default_songs
  ~frames_writer
  =
  let songs = Songs.create ~default_songs in
  let skip = Bvar.create () in
  let on_song_start = Bvar.create () in
  let on_songs_empty = Bvar.create () in
  let on_new_frames_writer = Bvar.create () in
  let started = Set_once.create () in
  let closed = Set_once.create () in
  { auth_token
  ; ffmpeg_path
  ; yt_dlp_path
  ; guild_id
  ; voice_channel
  ; message_channel
  ; frames_writer
  ; playing = None
  ; songs
  ; skip
  ; on_song_start
  ; on_songs_empty
  ; on_new_frames_writer
  ; started
  ; closed
  }
;;
