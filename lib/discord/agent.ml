open Util

type join_channel =
  { self_mute : bool
  ; self_deaf : bool
  ; guild_id : string
  ; channel_id : string
  }

type leave_channel = { guild_id : string }

type play_voice =
  { guild_id : string
  ; src :
      [ `Pipe of Eio.Flow.source_ty Eio.Resource.t
      | `Ytdl of string
      | `Bilibili of string
      ]
  }

type skip = { guild_id : string }

type get_voice_states =
  { guild_id : string
  ; user_id : string
  }

type consumer = Event.t Actaa.Gen_server.t_cast

type init_arg =
  { token : string
  ; intents : int
  ; consumer : consumer
  ; ffmpeg_path : string
  ; ffmpeg_options : string list
  ; youtubedl_path : string
  ; media_get_path : string
  }

type call_msg =
  [ `GetVoiceStates of get_voice_states
  | `GetAllVoiceStates of string (* guild_id *)
  | `Me
  ]

type call_reply =
  [ `GetVoiceStates of Event.dispatch_voice_state_update option
  | `GetAllVoiceStates of Event.dispatch_voice_state_update StringMap.t
  | `Me of Entity.user option
  ]

type cast_msg =
  [ `Event of Event.t
  | `JoinChannel of join_channel
  | `LeaveChannel of leave_channel
  | `PlayVoice of play_voice
  | `Skip of skip
  | `ForceResumeGateway
  ]

type basic_msg = (call_msg, call_reply, cast_msg) Actaa.Gen_server.basic_msg
type msg = basic_msg

type state =
  { st : State.t
  ; gw : Gateway.t
  ; consumer : consumer
  ; ffmpeg_path : string
  ; ffmpeg_options : string list
  ; youtubedl_path : string
  ; media_get_path : string
  }

let spawn_youtubedl process_mgr ~sw ~stdout ~path:(executable : string) url =
  Eio.Process.spawn
    ~sw
    process_mgr
    ~stdout
    ~executable
    [ executable
    ; "-f"
    ; "bestaudio"
    ; "-o"
    ; "-"
    ; "-q"
    ; "--no-warnings"
    ; "--no-continue"
    ; "--user-agent"
    ; "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) \
       Chrome/127.0.6533.103 Mobile Safari/537.36"
    ; url
    ]
;;

let spawn_media_get process_mgr ~sw ~temp_file ~path:(executable : string) url =
  Eio.Process.spawn
    ~sw
    process_mgr
    ~executable
    [ executable; "-t"; "audio"; "-o"; temp_file; "-l"; "silence"; "-u"; url ]
;;

let spawn_ffmpeg process_mgr ~sw ~stdin ~stdout ~path:(executable : string) ~options =
  Eio.Process.spawn ~sw process_mgr ~stdin ~stdout ~executable (executable :: options)
;;

class t =
  object (self)
    inherit [init_arg, msg, state] Actaa.Gen_server.behaviour

    method
      private init
      env
      ~sw
      { token
      ; intents
      ; consumer
      ; ffmpeg_path
      ; ffmpeg_options
      ; youtubedl_path
      ; media_get_path
      } =
      let st = State.start env ~sw in
      let gw =
        Gateway.spawn
          env
          ~sw
          ~token
          ~intents
          ~state:st
          ~consumer:(self :> Gateway.consumer)
      in
      { st; gw; consumer; ffmpeg_path; ffmpeg_options; youtubedl_path; media_get_path }

    method! private handle_cast env ~sw ({ st; gw; consumer; _ } as state) =
      function
      | `Event ev ->
        Actaa.Gen_server.cast consumer ev;
        `NoReply state
      | `JoinChannel { self_mute; self_deaf; guild_id; channel_id } ->
        Gateway.send_voice_state_update ~guild_id ~channel_id ~self_mute ~self_deaf gw;
        `NoReply state
      | `LeaveChannel { guild_id } ->
        Gateway.send_voice_state_update ~guild_id ~self_mute:false ~self_deaf:false gw;
        `NoReply state
      | `Skip { guild_id } ->
        (match State.voice st guild_id with
         | None -> Logs.warn (fun m -> m "VoiceGateway is not available")
         | Some gateway -> Voice_gateway.skip gateway);
        `NoReply state
      | `PlayVoice { guild_id; src } ->
        (match State.voice st guild_id with
         | None -> Logs.warn (fun m -> m "VoiceGateway is not available")
         | Some gateway ->
           let process_mgr = Eio.Stdenv.process_mgr env in
           let play src =
             let src', sink' = Eio.Process.pipe ~sw process_mgr in
             let _ =
               spawn_ffmpeg
                 process_mgr
                 ~sw
                 ~stdin:src
                 ~stdout:sink'
                 ~path:state.ffmpeg_path
                 ~options:state.ffmpeg_options
             in
             Eio.Flow.close sink';
             Voice_gateway.send_frame_source
               gateway
               (src' :> Eio.Flow.source_ty Eio.Resource.t)
           in
           (match src with
            | `Pipe (src : Eio.Flow.source_ty Eio.Resource.t) -> play src
            | `Ytdl url ->
              let src, sink = Eio.Process.pipe ~sw process_mgr in
              let _ =
                spawn_youtubedl
                  process_mgr
                  ~sw
                  ~stdout:sink
                  ~path:state.youtubedl_path
                  url
              in
              play src;
              Eio.Flow.close src;
              Eio.Flow.close sink
            | `Bilibili url ->
              let rec retry () =
                try
                  let temp_file =
                    Filename.get_temp_dir_name () ^ "/yum-download-" ^ guild_id ^ ".mp3"
                  in
                  let rec retry () =
                    let process =
                      spawn_media_get
                        process_mgr
                        ~sw
                        ~temp_file
                        ~path:state.media_get_path
                        url
                    in
                    match Eio.Process.await process with
                    | `Exited code when code = 0 -> ()
                    | status ->
                      Eio.Process.pp_status Format.err_formatter status;
                      retry ()
                  in
                  retry ();
                  let fs = Eio.Stdenv.fs env in
                  let src = Eio.Path.(open_in ~sw (fs / temp_file)) in
                  play src
                with
                | e ->
                  Logs.err (fun m ->
                    m
                      "Download failed, retrying... : %s: %s"
                      (Printexc.to_string e)
                      (Printexc.get_backtrace ()));
                  retry ()
              in
              retry ()));
        `NoReply state
      | `ForceResumeGateway ->
        Gateway.force_resume gw;
        `NoReply state

    method! private handle_call _env ~sw:_ _from state =
      function
      | `GetVoiceStates { guild_id; user_id } ->
        `Reply (`GetVoiceStates (State.voice_states state.st ~guild_id ~user_id), state)
      | `GetAllVoiceStates guild_id ->
        `Reply (`GetAllVoiceStates (State.all_voice_states state.st ~guild_id), state)
      | `Me -> `Reply (`Me (State.me state.st), state)
  end

let join_channel
      ?(self_mute = false)
      ?(self_deaf = false)
      ~guild_id
      ~channel_id
      (agent : t)
  =
  Actaa.Gen_server.cast
    agent
    (`JoinChannel { guild_id; channel_id; self_mute; self_deaf })
;;

let leave_channel ~guild_id (agent : t) =
  Actaa.Gen_server.cast agent (`LeaveChannel { guild_id })
;;

let get_voice_states ~guild_id ~user_id (agent : t) =
  match Actaa.Gen_server.call agent (`GetVoiceStates { guild_id; user_id }) with
  | `GetVoiceStates v -> v
  | _ -> assert false
;;

let play_voice ~guild_id ~src (agent : t) =
  Actaa.Gen_server.cast agent (`PlayVoice { guild_id; src })
;;

let skip ~guild_id (agent : t) = Actaa.Gen_server.cast agent (`Skip { guild_id })

let me (agent : t) =
  match Actaa.Gen_server.call agent `Me with
  | `Me v -> v
  | _ -> assert false
;;

let get_all_voice_states (agent : t) ~guild_id =
  match Actaa.Gen_server.call agent (`GetAllVoiceStates guild_id) with
  | `GetAllVoiceStates v -> v
  | _ -> assert false
;;

let force_resume (agent : t) = Actaa.Gen_server.cast agent `ForceResumeGateway
