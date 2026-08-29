open! Core
open! Async

let bilibili_sessdata_flag =
  Command.Param.flag
    "-bilibili-sessdata"
    ~doc:
      "STRING SESSDATA cookie of a logged-in Bilibili account, used to clear search risk \
       control (required to search from a datacenter/VPS IP)"
    Command.Param.(optional string)
;;

let run_command =
  Command.async_or_error
    ~summary:"😋 Run the Discord music player bot."
    (let%map_open.Command () = Log.set_level_via_param (force Log.Global.log)
     and discord_bot_token =
       flag
         [%var_dash_name]
         ~doc:"STRING Discord bot auth token"
         (required Discord.Model.Auth_token.arg_type)
     and youtube_songs =
       flag [%var_dash_name] ~doc:"FILE Youtube songs file" (required string)
     and ffmpeg_path =
       flag_optional_with_default_doc_string
         [%var_dash_name]
         File_path.Absolute.arg_type
         File_path.Absolute.to_string
         ~default:Ffmpeg.default_prog
         ~doc:"PATH Path to the ffmpeg binary"
     and yt_dlp_path =
       flag_optional_with_default_doc_string
         [%var_dash_name]
         File_path.Absolute.arg_type
         File_path.Absolute.to_string
         ~default:Youtube.default_prog
         ~doc:"PATH Path to the yt-dlp binary"
     and yt_dlp_cookies =
       flag
         [%var_dash_name]
         ~doc:
           "FILE Netscape-format cookie file passed to yt-dlp via --cookies (e.g. to \
            clear bot checks)"
         (optional File_path.arg_type)
     and bilibili_sessdata = bilibili_sessdata_flag in
     fun () ->
       Server.run
         ~discord_bot_token
         ~youtube_songs
         ~ffmpeg_path
         ~youtube:(Youtube.create ~prog:yt_dlp_path ?cookies:yt_dlp_cookies ())
         ~bilibili_sessdata
         ())
;;

let search_test_command =
  Command.async_or_error
    ~summary:
      "🧪 Run a one-off Bilibili keyword search and print the results. Handy for checking \
       whether a host's IP and -bilibili-sessdata cookie are accepted by Bilibili search \
       before running the bot."
    (let%map_open.Command () = Log.set_level_via_param (force Log.Global.log)
     and max_results =
       flag
         [%var_dash_name]
         ~doc:"N Maximum number of results (default 5)"
         (optional_with_default 5 int)
     and repeat =
       flag
         [%var_dash_name]
         ~doc:
           "N Run the search N times and report how many succeeded. Risk control is \
            intermittent, so a single run tells you very little (default 1)"
         (optional_with_default 1 int)
     and sessdata = bilibili_sessdata_flag
     and query = anon ("KEYWORD" %: string) in
     fun () ->
       let print_results results =
         printf
           "✅ Bilibili search works from this host: %d result(s)\n"
           (List.length results);
         List.iter
           results
           ~f:(fun { Bilibili.Search_result.bvid; title; author; duration } ->
             printf
               "  %-12s  %-7s  %-16s  %s\n"
               bvid
               (Option.value duration ~default:"-")
               (Option.value author ~default:"-")
               title)
       in
       (* Sequential on purpose: the point is to sample risk control over time the
          way real searches arrive, and to let the runs share one primed session. *)
       let%map outcomes =
         Deferred.List.init repeat ~how:`Sequential ~f:(fun (_ : int) ->
           Bilibili.search ?sessdata ~max_results query)
       in
       Option.iter (List.find_map outcomes ~f:Result.ok) ~f:print_results;
       if repeat > 1
       then
         printf
           "\n%d/%d searches succeeded\n"
           (List.count outcomes ~f:Or_error.is_ok)
           repeat;
       match List.filter_map outcomes ~f:Result.error with
       | errors when List.length errors = repeat ->
         eprintf
           "❌ Bilibili search FAILED. Risk control here is rate-based on the IP and \
            account rather than a fixed property of the host: a valid \
            -bilibili-sessdata cookie helps most, and a block usually clears on its \
            own after a pause.\n";
         Error (Error.of_list errors)
       | _ -> Ok ())
;;

let command =
  Command.group
    ~summary:"😋 A Discord music player bot."
    [ "run", run_command; "search-test", search_test_command ]
;;
