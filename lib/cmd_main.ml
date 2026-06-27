open! Core
open! Async

let bilibili_sessdata_flag =
  Command.Param.flag
    "-bilibili-sessdata"
    ~doc:
      "STRING SESSDATA cookie of a logged-in Bilibili account, used to clear \
       search risk control (required to search from a datacenter/VPS IP)"
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
         ~default:Yt_dlp.default_prog
         ~doc:"PATH Path to the yt-dlp binary"
     and bilibili_sessdata = bilibili_sessdata_flag in
     fun () ->
       Server.run
         ~discord_bot_token
         ~youtube_songs
         ~ffmpeg_path
         ~yt_dlp_path
         ~bilibili_sessdata
         ())
;;

let search_test_command =
  Command.async_or_error
    ~summary:
      "🧪 Run a one-off Bilibili keyword search and print the results. Handy for \
       checking whether a host's IP and -bilibili-sessdata cookie are accepted \
       by Bilibili search before running the bot."
    (let%map_open.Command () = Log.set_level_via_param (force Log.Global.log)
     and max_results =
       flag
         [%var_dash_name]
         ~doc:"N Maximum number of results (default 5)"
         (optional_with_default 5 int)
     and sessdata = bilibili_sessdata_flag
     and query = anon ("KEYWORD" %: string) in
     fun () ->
       match%map Bilibili.search ?sessdata ~max_results query with
       | Ok results ->
         printf "✅ Bilibili search works from this host: %d result(s)\n" (List.length results);
         List.iter results ~f:(fun { bvid; title; author; duration } ->
           printf
             "  %-12s  %-7s  %-16s  %s\n"
             bvid
             (Option.value duration ~default:"-")
             (Option.value author ~default:"-")
             title);
         Ok ()
       | Error error ->
         eprintf
           "❌ Bilibili search FAILED from this host. The IP is most likely \
            geo-blocked / risk-controlled; try a mainland-China host.\n";
         Error error)
;;

let command =
  Command.group
    ~summary:"😋 A Discord music player bot."
    [ "run", run_command; "search-test", search_test_command ]
;;
