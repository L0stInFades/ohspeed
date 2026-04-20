type entry = {
  recorded_at : float;
  preset : Model.preset;
  server : Model.server_meta;
  idle_latency_ms : float option;
  idle_jitter_ms : float option;
  download_bps : float option;
  upload_bps : float option;
  download_loaded_latency_ms : float option;
  upload_loaded_latency_ms : float option;
}

let default_retain = 200

let json_of_option json_of = function
  | None -> `Null
  | Some value -> json_of value

let server_to_yojson (server : Model.server_meta) =
  `Assoc
    [
      ("colo", json_of_option (fun value -> `String value) server.colo);
      ("city", json_of_option (fun value -> `String value) server.city);
      ("country", json_of_option (fun value -> `String value) server.country);
      ("asn", json_of_option (fun value -> `String value) server.asn);
      ("ip", json_of_option (fun value -> `String value) server.ip);
    ]

let string_option_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Some value
  | _ -> None

let float_option_field json name =
  match Yojson.Safe.Util.member name json with
  | `Float value -> Some value
  | `Int value -> Some (float_of_int value)
  | `Intlit value -> float_of_string_opt value
  | _ -> None

let server_of_yojson json =
  {
    Model.colo = string_option_field json "colo";
    city = string_option_field json "city";
    country = string_option_field json "country";
    asn = string_option_field json "asn";
    ip = string_option_field json "ip";
  }

let entry_to_yojson entry =
  `Assoc
    [
      ("recorded_at", `Float entry.recorded_at);
      ("preset", `String (Model.preset_to_string entry.preset));
      ("server", server_to_yojson entry.server);
      ("idle_latency_ms", json_of_option (fun value -> `Float value) entry.idle_latency_ms);
      ("idle_jitter_ms", json_of_option (fun value -> `Float value) entry.idle_jitter_ms);
      ("download_bps", json_of_option (fun value -> `Float value) entry.download_bps);
      ("upload_bps", json_of_option (fun value -> `Float value) entry.upload_bps);
      ( "download_loaded_latency_ms",
        json_of_option (fun value -> `Float value) entry.download_loaded_latency_ms );
      ( "upload_loaded_latency_ms",
        json_of_option (fun value -> `Float value) entry.upload_loaded_latency_ms );
    ]

let entry_of_yojson json =
  match Yojson.Safe.Util.member "preset" json with
  | `String preset -> (
      match Model.preset_of_string preset with
      | Error _ -> None
      | Ok preset ->
          let recorded_at =
            match Yojson.Safe.Util.member "recorded_at" json with
            | `Float value -> value
            | `Int value -> float_of_int value
            | `Intlit value -> float_of_string value
            | _ -> 0.
          in
          let server =
            match Yojson.Safe.Util.member "server" json with
            | `Assoc _ as server -> server_of_yojson server
            | _ -> Model.empty_server_meta
          in
          Some
            {
              recorded_at;
              preset;
              server;
              idle_latency_ms = float_option_field json "idle_latency_ms";
              idle_jitter_ms = float_option_field json "idle_jitter_ms";
              download_bps = float_option_field json "download_bps";
              upload_bps = float_option_field json "upload_bps";
              download_loaded_latency_ms =
                float_option_field json "download_loaded_latency_ms";
              upload_loaded_latency_ms =
                float_option_field json "upload_loaded_latency_ms";
            })
  | _ -> None

let home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some home when home <> "" -> home
  | _ -> "."

let state_dir_name = "ohspeed"

let legacy_state_dir_name = "netprobe"

let state_dir () =
  match Sys.getenv_opt "XDG_STATE_HOME" with
  | Some dir when dir <> "" -> Filename.concat dir state_dir_name
  | _ ->
      Filename.concat (Filename.concat (home_dir ()) ".local/state") state_dir_name

let legacy_state_dir () =
  match Sys.getenv_opt "XDG_STATE_HOME" with
  | Some dir when dir <> "" -> Filename.concat dir legacy_state_dir_name
  | _ ->
      Filename.concat (Filename.concat (home_dir ()) ".local/state")
        legacy_state_dir_name

let file_path () = Filename.concat (state_dir ()) "history.jsonl"

let legacy_file_path () = Filename.concat (legacy_state_dir ()) "history.jsonl"

let rec ensure_dir path =
  if Sys.file_exists path then
    ()
  else
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755

let of_report (report : Model.report) =
  {
    recorded_at = report.generated_at;
    preset = report.preset;
    server = report.server;
    idle_latency_ms = report.idle_latency_ms;
    idle_jitter_ms = report.idle_jitter_ms;
    download_bps = Option.bind report.download (fun direction -> direction.bandwidth_bps);
    upload_bps = Option.bind report.upload (fun direction -> direction.bandwidth_bps);
    download_loaded_latency_ms =
      Option.bind report.download (fun direction -> direction.loaded_latency_ms);
    upload_loaded_latency_ms =
      Option.bind report.upload (fun direction -> direction.loaded_latency_ms);
  }

let load_lines path =
  if not (Sys.file_exists path) then
    []
  else
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let rec loop acc =
          match input_line channel with
          | line -> loop (line :: acc)
          | exception End_of_file -> List.rev acc
        in
        loop [])

let take_last count values =
  let total = List.length values in
  let drop = max 0 (total - count) in
  let rec skip remaining = function
    | [] -> []
    | values when remaining <= 0 -> values
    | _ :: rest -> skip (remaining - 1) rest
  in
  skip drop values

let load ?limit () =
  let path =
    let primary = file_path () in
    if Sys.file_exists primary then
      primary
    else
      legacy_file_path ()
  in
  let entries =
    load_lines path
    |> List.filter_map (fun line ->
           try Yojson.Safe.from_string line |> entry_of_yojson
           with _ -> None)
  in
  match limit with
  | None -> entries
  | Some limit when limit > 0 -> take_last limit entries
  | Some _ -> []

let write_entries path entries =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      List.iter
        (fun entry ->
          Yojson.Safe.to_string (entry_to_yojson entry) |> output_string channel;
          output_char channel '\n')
        entries)

let append ?(retain = default_retain) report =
  try
    let path = file_path () in
    ensure_dir (Filename.dirname path);
    let entries = load () @ [ of_report report ] |> take_last retain in
    write_entries path entries;
    Ok ()
  with exn -> Error (Printexc.to_string exn)
