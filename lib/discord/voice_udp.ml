open! Core
open! Async
open! Common

type t =
  { ssrc : Model.Ssrc.t
  ; socket : (([ `Bound ], Socket.Address.Inet.t) Socket.t[@sexp.opaque])
  ; dest : Socket.Address.Inet.t
  ; sendto :
      (Fd.t -> (read, Iobuf.seek, Iobuf.global) Iobuf.t -> Socket.Address.Inet.t -> unit
      [@sexp.opaque])
  ; send_speaking :
      (Model.Voice_gateway.Event.Speaking.t -> unit Deferred.t[@sexp.opaque])
  ; tls_encryptor : Tls_encryptor.t
  ; opus_encoder : (Audio.Opus.Encoder.t[@sexp.opaque])
  ; mutable seq_num : int
  ; mutable timestamp : int
  ; mutable nonce : int
  ; closed : unit Ivar.t
  }
[@@deriving sexp_of, fields ~getters]

let create ~ssrc ~ip ~port ~tls_encryption_modes ~send_speaking =
  Sodium.init () |> ok_exn;
  let%bind.Deferred.Or_error tls_encryptor =
    Tls_encryptor.create tls_encryption_modes |> return
  in
  let%bind.Deferred.Or_error sendto = Async_udp.sendto_sync () |> return in
  let sendto fd iobuf addr = sendto fd iobuf addr |> Unix.Syscall_result.Unit.ok_exn in
  let socket = Socket.Address.Inet.create_bind_any ~port:0 |> Async_udp.bind in
  [%log.debug
    [%here]
      "Bound voice UDP socket"
      ~addr:(Socket.getsockname socket : Socket.Address.Inet.t)];
  let dest = Socket.Address.Inet.create (Unix.Inet_addr.of_string ip) ~port in
  let%map.Deferred.Or_error opus_encoder =
    Audio.Opus.Encoder.create ~application:Voip |> return
  in
  { ssrc
  ; socket
  ; dest
  ; sendto
  ; send_speaking
  ; tls_encryptor
  ; opus_encoder
  ; seq_num = 0
  ; timestamp = 0
  ; nonce = 0
  ; closed = Ivar.create ()
  }
;;

let close
  { ssrc = _
  ; socket
  ; dest = _
  ; sendto = _
  ; send_speaking = _
  ; tls_encryptor = _
  ; opus_encoder
  ; seq_num = _
  ; timestamp = _
  ; nonce = _
  ; closed
  }
  =
  Ivar.fill_if_empty closed ();
  Socket.shutdown socket `Both;
  Audio.Opus.Encoder.destroy opus_encoder
;;

let tls_encryption_mode t = Tls_encryptor.mode t.tls_encryptor

(** https://discord.com/developers/docs/topics/voice-connections#ip-discovery *)
let discover_ip t =
  let ret = Ivar.create () in
  Async_udp.read_loop
    ~config:(Async_udp.Config.create ~stop:(Ivar.read ret |> Deferred.ignore_m) ())
    (Socket.fd t.socket)
    (fun response ->
      [%log.debug [%here] "Received IP discovery response"];
      let result =
        match Iobuf.length response with
        | 74 ->
          let my_ip =
            Iobuf.Peek.stringo ~len:64 response ~pos:8
            |> String.take_while ~f:(function
              | '\000' -> false
              | _ -> true)
          in
          let my_port = Iobuf.Peek.uint16_be response ~pos:72 in
          Ok (~my_ip, ~my_port)
        | n ->
          Error
            (Error.create_s
               [%message
                 "Expecting 74-byte response for IP discovery but have received another \
                  size"
                   ~num_bytes:(n : int)
                   ~bytes:(Iobuf.to_bytes response : Bytes.t)])
      in
      Ivar.fill_if_empty ret result)
  |> Deferred.ignore_m
  |> don't_wait_for;
  let request =
    let buf = Iobuf.create ~len:74 in
    Iobuf.Fill.uint16_be_trunc buf 0x01;
    Iobuf.Fill.uint16_be_trunc buf 70;
    Iobuf.Fill.uint32_be_trunc buf (Model.Ssrc.to_int_exn t.ssrc);
    Iobuf.rewind buf;
    Iobuf.read_only buf
  in
  [%log.debug
    [%here] "Sending IP discovery request" ~dest:(t.dest : Socket.Address.Inet.t)];
  t.sendto (Socket.fd t.socket) request t.dest;
  [%log.debug [%here] "Sent IP discovery request" ~dest:(t.dest : Socket.Address.Inet.t)];
  Ivar.read ret
;;

module State = struct
  let max_idle_duration = Audio.Pcm_frame.frame_duration

  type state =
    | Not_playing
    | Playing of
        { start_time : Time_ns.t
        ; mutable sent_frames : int
        }

  type t =
    { mutable state : state
    ; on_change : [ `Playing | `Not_playing ] -> unit Deferred.t
    }

  let create ~on_change = { state = Not_playing; on_change }

  let start t ~sent_frames =
    match t.state with
    | Playing _ -> return ()
    | Not_playing ->
      t.state <- Playing { start_time = Time_ns.now (); sent_frames };
      t.on_change `Playing
  ;;

  let stop t =
    match t.state with
    | Not_playing -> return ()
    | Playing { start_time = _; sent_frames = _ } ->
      t.state <- Not_playing;
      t.on_change `Not_playing
  ;;

  let on_waiting_for_frames t =
    match t.state with
    | Not_playing -> ()
    | Playing { start_time; sent_frames } ->
      don't_wait_for
        (let%bind () = Clock_ns.after max_idle_duration in
         match t.state with
         | Not_playing -> return ()
         | Playing { start_time = start_time'; sent_frames = sent_frames' } ->
           if [%equal: Time_ns.t] start_time start_time'
              && [%equal: int] sent_frames sent_frames'
           then stop t
           else return ())
  ;;

  let on_sending_frames t ~num_frames =
    match t.state with
    | Not_playing -> start t ~sent_frames:num_frames
    | Playing ({ start_time; sent_frames } as playing) ->
      let expected_elapsed =
        Time_ns.Span.(scale_int Audio.Pcm_frame.frame_duration sent_frames)
      in
      let actual_elapsed = Time_ns.diff (Time_ns.now ()) start_time in
      let song_elapsed_diff =
        Time_ns.Span.(max zero (actual_elapsed - expected_elapsed))
      in
      playing.sent_frames <- sent_frames + num_frames;
      Clock_ns.after
        Time_ns.Span.(
          scale_int Audio.Pcm_frame.frame_duration num_frames - song_elapsed_diff)
  ;;
end

let header ~seq_num ~timestamp ~ssrc =
  let buf = Iobuf.create ~len:12 in
  Iobuf.Fill.uint8_trunc buf 0x80;
  Iobuf.Fill.uint8_trunc buf 0x78;
  Iobuf.Fill.uint16_be_trunc buf seq_num;
  Iobuf.Fill.uint32_be_trunc buf timestamp;
  Iobuf.Fill.uint32_be_trunc buf (Model.Ssrc.to_int_exn ssrc);
  Iobuf.flip_lo buf;
  Iobuf.to_bytes buf
;;

(** https://discord.com/developers/docs/topics/voice-connections#transport-encryption-modes-voice-packet-structure *)
let send_frame t frame ~tls_secret_key =
  let header = header ~seq_num:t.seq_num ~timestamp:t.timestamp ~ssrc:t.ssrc in
  let packet =
    Tls_encryptor.encrypt
      t.tls_encryptor
      ~header
      ~frame
      ~secret_key:tls_secret_key
      ~nonce:t.nonce
  in
  t.seq_num <- t.seq_num + 1;
  t.nonce <- t.nonce + 1;
  t.timestamp <- t.timestamp + Audio.Pcm_frame.samples_per_frame;
  t.sendto (Socket.fd t.socket) packet t.dest
;;

let send_five_silent_frames =
  let frames = Array.create ~len:5 Audio.Opus.Frame.silence in
  fun send_frame -> Array.iter frames ~f:send_frame
;;

let send_speaking t is_speaking =
  (* https://discord.com/developers/docs/topics/voice-connections#speaking *)
  let speaking = if is_speaking then 1 else 0 in
  t.send_speaking { speaking; delay = 0; ssrc = t.ssrc }
;;

let frames_writer t ~tls_secret_key ~dave_session =
  let mls_encrypt opus_frame =
    Dave_session.encrypt
      dave_session
      ~ssrc:t.ssrc
      ~plaintext:(Audio.Opus.Frame.to_bytes opus_frame)
  in
  let send_frame = send_frame t ~tls_secret_key in
  let state =
    State.create ~on_change:(function
      | `Playing -> send_speaking t true
      | `Not_playing ->
        if Ivar.is_full t.closed
        then return ()
        else (
          send_five_silent_frames (Fn.compose send_frame mls_encrypt);
          send_speaking t false))
  in
  let reader, writer = Pipe.create () in
  let rec loop () =
    State.on_waiting_for_frames state;
    match%bind Pipe.read reader with
    | `Eof ->
      [%log.debug
        [%here]
          "Frames writer closed, closing voice UDP..."
          ~dest:(t.dest : Socket.Address.Inet.t)];
      close t;
      return ()
    | `Ok pcm_frames ->
      let num_frames = Queue.length pcm_frames in
      let mls_encrypted_frames =
        Queue.map pcm_frames ~f:(fun pcm ->
          Audio.Opus.Encoder.encode t.opus_encoder pcm |> ok_exn |> mls_encrypt)
      in
      let%bind () = State.on_sending_frames state ~num_frames in
      Queue.iter mls_encrypted_frames ~f:send_frame;
      loop ()
  in
  don't_wait_for (loop ());
  writer
;;

module%test _ = struct
  let print_bytes_hex bytes =
    Bytes.to_list bytes |> List.iter ~f:(fun b -> printf "%02x" (Char.to_int b))
  ;;

  let%expect_test "header" =
    let header = header ~seq_num:20 ~timestamp:960 ~ssrc:(Model.Ssrc.of_int_exn 1000) in
    print_bytes_hex header;
    [%expect {| 80780014000003c0000003e8 |}];
    return ()
  ;;
end
