open Model

let human_bps bps =
  let units =
    [|
      ("bps", 1.);
      ("Kbps", 1_000.);
      ("Mbps", 1_000_000.);
      ("Gbps", 1_000_000_000.);
    |]
  in
  let rec choose index =
    if index = Array.length units - 1 || bps < snd units.(index + 1) then
      units.(index)
    else
      choose (index + 1)
  in
  let unit_name, divisor = choose 0 in
  Fmt.str "%.2f %s" (bps /. divisor) unit_name

let human_bytes bytes =
  let units =
    [|
      ("B", 1.);
      ("KiB", 1024.);
      ("MiB", 1024. *. 1024.);
      ("GiB", 1024. *. 1024. *. 1024.);
    |]
  in
  let rec choose index =
    if index = Array.length units - 1 || float_of_int bytes < snd units.(index + 1) then
      units.(index)
    else
      choose (index + 1)
  in
  let unit_name, divisor = choose 0 in
  Fmt.str "%.2f %s" (float_of_int bytes /. divisor) unit_name

let maybe_ms = function
  | None -> "n/a"
  | Some value -> Fmt.str "%.2f ms" value

let maybe_bps = function
  | None -> "n/a"
  | Some value -> human_bps value

let progress_bar ~width completed total =
  if total <= 0 then
    "[----------]"
  else
    let ratio = float_of_int completed /. float_of_int total in
    let filled =
      int_of_float
        (Float.round (Float.max 0. (Float.min 1. ratio) *. float_of_int width))
    in
    let filled = Int.min width filled in
    "[" ^ String.make filled '#' ^ String.make (width - filled) '-' ^ "]"

let elapsed_string started_at =
  Fmt.str "%.1fs" (Unix.gettimeofday () -. started_at)

let timestamp_string timestamp =
  let tm = Unix.localtime timestamp in
  Fmt.str "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min
    tm.tm_sec

let present_fields values =
  List.filter_map (fun value -> value) values

let server_string server =
  let location =
    present_fields
      [
        server.colo;
        server.city;
        server.country;
        Option.map (fun asn -> "ASN " ^ asn) server.asn;
        Option.map (fun ip -> "IP " ^ ip) server.ip;
      ]
  in
  match location with
  | [] -> "unknown"
  | _ -> String.concat " / " location

let add_line buffer fmt =
  Printf.ksprintf
    (fun line ->
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n')
    fmt

let latest_sample_bps direction =
  match List.rev direction.samples with
  | [] -> None
  | sample :: _ -> Some sample.bps

let render_phase = function
  | Fetching_meta -> ("metadata lookup", None)
  | Idle_latency { completed; total } ->
      ( Fmt.str "idle latency %d/%d" completed total,
        Some (progress_bar ~width:24 completed total) )
  | Downloading { set_index; total_sets; request_bytes; completed; total } ->
      ( Fmt.str "download set %d/%d @ %s" set_index total_sets
          (human_bytes request_bytes),
        Some (progress_bar ~width:24 completed total) )
  | Uploading { set_index; total_sets; request_bytes; completed; total } ->
      ( Fmt.str "upload set %d/%d @ %s" set_index total_sets
          (human_bytes request_bytes),
        Some (progress_bar ~width:24 completed total) )
  | Finished -> ("finished", None)

let render_direction buffer label direction =
  match direction with
  | None -> add_line buffer "%-16s skipped" label
  | Some direction ->
      add_line buffer "%-16s %s" label (maybe_bps direction.bandwidth_bps);
      add_line buffer "%-16s %s" (label ^ " loaded") (maybe_ms direction.loaded_latency_ms);
      add_line buffer "%-16s %s" (label ^ " jitter") (maybe_ms direction.loaded_jitter_ms);
      add_line buffer "%-16s %d / %d qualified"
        (label ^ " samples") direction.qualified_samples
        (List.length direction.samples);
      add_line buffer "%-16s %s"
        (label ^ " chunk")
        (match direction.selected_request_bytes with
        | None -> "n/a"
        | Some bytes -> human_bytes bytes)

let render_text (report : Model.report) =
  let buffer = Buffer.create 512 in
  add_line buffer "ohspeed";
  add_line buffer "%-16s %s" "Preset" (preset_to_string report.preset);
  add_line buffer "%-16s %s" "Timestamp" (timestamp_string report.generated_at);
  add_line buffer "%-16s %s" "Server" (server_string report.server);
  add_line buffer "";
  add_line buffer "%-16s %s" "Idle latency" (maybe_ms report.idle_latency_ms);
  add_line buffer "%-16s %s" "Idle jitter" (maybe_ms report.idle_jitter_ms);
  add_line buffer "%-16s %d" "Idle samples" (List.length report.idle_points);
  add_line buffer "";
  render_direction buffer "Download" report.download;
  add_line buffer "";
  render_direction buffer "Upload" report.upload;
  Buffer.contents buffer |> String.trim

let render_live_direction buffer label enabled direction =
  if not enabled then
    add_line buffer "%-16s skipped" label
  else
    match direction with
    | None -> add_line buffer "%-16s pending" label
    | Some direction ->
        add_line buffer "%-16s %s" label (maybe_bps direction.bandwidth_bps);
        add_line buffer "%-16s %s" (label ^ " latest")
          (maybe_bps (latest_sample_bps direction));
        add_line buffer "%-16s %s" (label ^ " loaded")
          (maybe_ms direction.loaded_latency_ms);
        add_line buffer "%-16s %s" (label ^ " jitter")
          (maybe_ms direction.loaded_jitter_ms);
        add_line buffer "%-16s %d / %d qualified"
          (label ^ " samples") direction.qualified_samples
          (List.length direction.samples);
        add_line buffer "%-16s %s"
          (label ^ " chunk")
          (match direction.selected_request_bytes with
          | None -> "n/a"
          | Some bytes -> human_bytes bytes)

let render_live (progress : Model.progress) =
  let buffer = Buffer.create 768 in
  let phase_text, phase_bar = render_phase progress.phase in
  add_line buffer "ohspeed Live";
  add_line buffer "%-16s %s" "Preset" (preset_to_string progress.preset);
  add_line buffer "%-16s %s" "Elapsed" (elapsed_string progress.started_at);
  add_line buffer "%-16s %s" "Phase" phase_text;
  (match phase_bar with
  | None -> ()
  | Some bar -> add_line buffer "%-16s %s" "Progress" bar);
  add_line buffer "%-16s %s" "Server" (server_string progress.server);
  add_line buffer "";
  add_line buffer "%-16s %s" "Idle latency"
    (maybe_ms (Stats.percentile progress.idle_points 0.5));
  add_line buffer "%-16s %s" "Idle jitter"
    (maybe_ms (Stats.jitter progress.idle_points));
  add_line buffer "%-16s %d" "Idle samples" (List.length progress.idle_points);
  add_line buffer "";
  render_live_direction buffer "Download" progress.download_enabled
    progress.download;
  add_line buffer "";
  render_live_direction buffer "Upload" progress.upload_enabled progress.upload;
  Buffer.contents buffer |> String.trim

let json_of_option json_of = function
  | None -> `Null
  | Some value -> json_of value

let json_of_latency_sample (sample : latency_sample) =
  `Assoc
    [
      ("ping_ms", `Float sample.ping_ms);
      ("status_code", `Int sample.status_code);
      ("timestamp_s", `Float sample.timestamp_s);
    ]

let json_of_bandwidth_sample (sample : bandwidth_sample) =
  `Assoc
    [
      ("request_bytes", `Int sample.request_bytes);
      ("transfer_bytes", `Int sample.transfer_bytes);
      ("duration_s", `Float sample.duration_s);
      ("bps", `Float sample.bps);
      ("ttfb_ms", `Float sample.ttfb_ms);
      ("status_code", `Int sample.status_code);
    ]

let json_of_bandwidth_set (set : bandwidth_set) =
  `Assoc
    [
      ("request_bytes", `Int set.request_bytes);
      ("min_duration_s", json_of_option (fun value -> `Float value) set.min_duration_s);
      ("samples", `List (List.map json_of_bandwidth_sample set.samples));
      ( "side_latencies",
        `List (List.map json_of_latency_sample set.side_latencies) );
    ]

let json_of_direction_result (direction : direction_result) =
  `Assoc
    [
      ("bandwidth_bps", json_of_option (fun value -> `Float value) direction.bandwidth_bps);
      ("qualified_samples", `Int direction.qualified_samples);
      ("selected_request_bytes", json_of_option (fun value -> `Int value) direction.selected_request_bytes);
      ("loaded_latency_ms", json_of_option (fun value -> `Float value) direction.loaded_latency_ms);
      ("loaded_jitter_ms", json_of_option (fun value -> `Float value) direction.loaded_jitter_ms);
      ("loaded_points", `List (List.map (fun value -> `Float value) direction.loaded_points));
      ("samples", `List (List.map json_of_bandwidth_sample direction.samples));
      ("sets", `List (List.map json_of_bandwidth_set direction.sets));
    ]

let json_of_server (server : server_meta) =
  `Assoc
    [
      ("colo", json_of_option (fun value -> `String value) server.colo);
      ("city", json_of_option (fun value -> `String value) server.city);
      ("country", json_of_option (fun value -> `String value) server.country);
      ("asn", json_of_option (fun value -> `String value) server.asn);
      ("ip", json_of_option (fun value -> `String value) server.ip);
    ]

let render_json (report : Model.report) =
  `Assoc
    [
      ("generated_at", `Float report.generated_at);
      ("timestamp", `String (timestamp_string report.generated_at));
      ("preset", `String (preset_to_string report.preset));
      ("server", json_of_server report.server);
      ("idle_latency_ms", json_of_option (fun value -> `Float value) report.idle_latency_ms);
      ("idle_jitter_ms", json_of_option (fun value -> `Float value) report.idle_jitter_ms);
      ("idle_points", `List (List.map (fun value -> `Float value) report.idle_points));
      ("download", json_of_option json_of_direction_result report.download);
      ("upload", json_of_option json_of_direction_result report.upload);
    ]
  |> Yojson.Safe.pretty_to_string
