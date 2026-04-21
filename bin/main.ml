open Cmdliner
open Ohspeed

let ( let* ) = Result.bind

let stdin_is_tty () =
  Unix.isatty (Unix.descr_of_in_channel stdin)

let stdout_is_tty () =
  Unix.isatty (Unix.descr_of_out_channel stdout)

let default_endpoints () =
  Model.
    {
      download_url = "https://speed.cloudflare.com/__down";
      upload_url = "https://speed.cloudflare.com/__up";
      meta_url = Some "https://speed.cloudflare.com/meta";
    }

let preset_conv =
  let parse value =
    match Model.preset_of_string value with
    | Ok preset -> Ok preset
    | Error msg -> Error (`Msg msg)
  in
  let print fmt preset =
    Format.pp_print_string fmt (Model.preset_to_string preset)
  in
  Arg.conv (parse, print)

let output_conv =
  let parse value =
    match Model.output_of_string value with
    | Ok output -> Ok output
    | Error msg -> Error (`Msg msg)
  in
  let print fmt output =
    Format.pp_print_string fmt (Model.output_to_string output)
  in
  Arg.conv (parse, print)

let positive_int name = function
  | None -> Ok None
  | Some value when value > 0 -> Ok (Some value)
  | Some _ -> Error (Printf.sprintf "%s must be greater than 0" name)

let positive_float name = function
  | None -> Ok None
  | Some value when value > 0. -> Ok (Some value)
  | Some _ -> Error (Printf.sprintf "%s must be greater than 0" name)

let parse_sizes name = function
  | None -> Ok None
  | Some csv ->
      Result.map
        (fun values -> Some values)
        (Model.parse_int_csv ~name csv)

let build_plan preset latency_samples download_parallel upload_parallel
    download_sizes upload_sizes timeout_s =
  let* latency_samples = positive_int "latency-count" latency_samples in
  let* download_parallel =
    positive_int "download-parallel" download_parallel
  in
  let* upload_parallel = positive_int "upload-parallel" upload_parallel in
  let* timeout_s = positive_float "timeout" timeout_s in
  let* download_sizes = parse_sizes "download-sizes" download_sizes in
  let* upload_sizes = parse_sizes "upload-sizes" upload_sizes in
  Ok
    Model.(
      plan_of_preset preset
      |> with_plan_overrides ?latency_samples ?download_parallel
           ?upload_parallel ?download_sizes ?upload_sizes ?request_timeout_s:
             timeout_s)

let run preset output download_url upload_url meta_url no_meta latency_samples
    download_parallel upload_parallel download_sizes upload_sizes timeout_s
    skip_download skip_upload live show_history history_limit no_save_history =
  let ansi = stdout_is_tty () in
  if skip_download && skip_upload then
    `Error (false, "At least one of download or upload must be enabled")
  else if live && output = Model.Json then
    `Error (false, "--live only supports text output")
  else if show_history && output = Model.Json then
    `Error (false, "--show-history only supports text output")
  else if live && show_history then
    `Error (false, "--live cannot be combined with --show-history")
  else if live && not (stdout_is_tty ()) then
    `Error (false, "--live requires a TTY stdout")
  else if show_history then
    let history = History.load ~limit:history_limit () in
    print_endline (Tui.render_history ~ansi ~animation_tick:0 ~history);
    `Ok ()
  else
    match
      build_plan preset latency_samples download_parallel upload_parallel
        download_sizes upload_sizes timeout_s
    with
    | Error msg -> `Error (false, msg)
    | Ok plan ->
        let endpoints =
          Model.
            {
              download_url;
              upload_url;
              meta_url =
                if no_meta then None else Some meta_url;
            }
        in
        let config =
          Model.
            {
              preset;
              plan;
              endpoints;
              enable_download = not skip_download;
              enable_upload = not skip_upload;
            }
        in
        let history = History.load ~limit:history_limit () in
        let session =
          if live then Some (Tui.create_session stdout) else None
        in
        let on_progress =
          match session with
          | None -> None
          | Some session ->
              Tui.start session;
              Some (fun progress ->
                  Tui.render session
                    (Tui.render_live ~ansi:true ~animation_tick:0 ~history
                       progress))
        in
        let result =
          Fun.protect
            ~finally:(fun () -> Option.iter Tui.stop session)
            (fun () ->
              try Ok (Measure.run ?on_progress config)
              with exn -> Error (Printexc.to_string exn))
        in
        (match result with
        | Error msg -> `Error (false, msg)
        | Ok report ->
            if not no_save_history then
              (match History.append report with
              | Ok () -> ()
              | Error msg -> prerr_endline ("ohspeed: failed to save history: " ^ msg));
            let rendered =
              match output with
              | Model.Text -> Report.render_text report
              | Model.Json -> Report.render_json report
            in
            print_endline rendered;
            `Ok ())

let preset_arg =
  let doc = "Measurement profile: quick, balanced, or full." in
  Arg.(value & opt preset_conv Model.Balanced & info [ "preset" ] ~doc)

let output_arg =
  let doc = "Output format: text or json." in
  Arg.(value & opt output_conv Model.Text & info [ "output" ] ~doc)

let download_url_arg =
  let doc = "Download endpoint. Must accept GET requests with bytes query." in
  Arg.(
    value
    & opt string "https://speed.cloudflare.com/__down"
    & info [ "download-url" ] ~docv:"URL" ~doc)

let upload_url_arg =
  let doc = "Upload endpoint. Must accept POST request bodies." in
  Arg.(
    value
    & opt string "https://speed.cloudflare.com/__up"
    & info [ "upload-url" ] ~docv:"URL" ~doc)

let meta_url_arg =
  let doc = "Metadata endpoint used to resolve server location." in
  Arg.(
    value
    & opt string "https://speed.cloudflare.com/meta"
    & info [ "meta-url" ] ~docv:"URL" ~doc)

let no_meta_arg =
  let doc = "Disable metadata lookup." in
  Arg.(value & flag & info [ "no-meta" ] ~doc)

let latency_count_arg =
  let doc = "Override latency sample count." in
  Arg.(value & opt (some int) None & info [ "latency-count" ] ~docv:"N" ~doc)

let download_parallel_arg =
  let doc = "Override concurrent download request count per measurement set." in
  Arg.(
    value
    & opt (some int) None
    & info [ "download-parallel" ] ~docv:"N" ~doc)

let upload_parallel_arg =
  let doc = "Override concurrent upload request count per measurement set." in
  Arg.(
    value
    & opt (some int) None
    & info [ "upload-parallel" ] ~docv:"N" ~doc)

let download_sizes_arg =
  let doc =
    "Override download request sizes in bytes, comma separated, e.g. \
     250000,1000000,8000000."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "download-sizes" ] ~docv:"CSV" ~doc)

let upload_sizes_arg =
  let doc =
    "Override upload request sizes in bytes, comma separated, e.g. \
     250000,1000000,4000000."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "upload-sizes" ] ~docv:"CSV" ~doc)

let timeout_arg =
  let doc = "Override per-request timeout in seconds." in
  Arg.(value & opt (some float) None & info [ "timeout" ] ~docv:"SECONDS" ~doc)

let skip_download_arg =
  let doc = "Skip download measurement." in
  Arg.(value & flag & info [ "skip-download" ] ~doc)

let skip_upload_arg =
  let doc = "Skip upload measurement." in
  Arg.(value & flag & info [ "skip-upload" ] ~doc)

let live_arg =
  let doc = "Render a real-time terminal dashboard while measuring." in
  Arg.(value & flag & info [ "live" ] ~doc)

let show_history_arg =
  let doc = "Render the saved history dashboard and exit." in
  Arg.(value & flag & info [ "show-history" ] ~doc)

let history_limit_arg =
  let doc = "How many recent runs to load for comparison panels." in
  Arg.(value & opt int 20 & info [ "history-limit" ] ~docv:"N" ~doc)

let no_save_history_arg =
  let doc = "Do not persist the finished run to history." in
  Arg.(value & flag & info [ "no-save-history" ] ~doc)

let cmd =
  let doc = "speedtest-grade OCaml CLI for quick network measurement" in
  let info = Cmd.info "ohspeed" ~version:"0.5.6" ~doc in
  let term =
    Term.(
      ret
        (const run $ preset_arg $ output_arg $ download_url_arg $ upload_url_arg
       $ meta_url_arg $ no_meta_arg $ latency_count_arg $ download_parallel_arg
       $ upload_parallel_arg $ download_sizes_arg $ upload_sizes_arg
       $ timeout_arg $ skip_download_arg $ skip_upload_arg $ live_arg
       $ show_history_arg $ history_limit_arg $ no_save_history_arg))
  in
  Cmd.v info term

let wants_interactive_tui () =
  match Array.to_list Sys.argv with
  | [ _ ] -> stdin_is_tty () && stdout_is_tty ()
  | [ _; "--tui" ] | [ _; "tui" ] -> true
  | _ -> false

let () =
  if wants_interactive_tui () then
    match App.run_interactive ~endpoints:(default_endpoints ()) () with
    | Ok () -> exit 0
    | Error message ->
        prerr_endline ("ohspeed: " ^ message);
        exit 1
  else
    exit (Cmd.eval cmd)
