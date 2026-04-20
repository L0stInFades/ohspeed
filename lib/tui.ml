open Model

type session = {
  out : out_channel;
  mutable started : bool;
  mutable last_frame : string option;
}

type metric_kind =
  | Bandwidth
  | Latency

let esc code = "\027[" ^ code
let reset = esc "0m"
let bold = esc "1m"
let dim = esc "2m"

let color256 code text = esc (Printf.sprintf "38;5;%dm" code) ^ text ^ reset
let bg256 code text = esc (Printf.sprintf "48;5;%dm" code) ^ text ^ reset
let style ~ansi code text = if ansi then code ^ text ^ reset else text
let colorize ~ansi code text = if ansi then color256 code text else text
let dim_text ~ansi text = if ansi then dim ^ text ^ reset else text
let bold_text ~ansi text = if ansi then bold ^ text ^ reset else text

let repeat s count =
  let buffer = Buffer.create (max 0 count * String.length s) in
  for _ = 1 to count do
    Buffer.add_string buffer s
  done;
  Buffer.contents buffer

let next_utf8_index s index =
  let byte = Char.code (String.get s index) in
  if byte land 0b1000_0000 = 0 then index + 1
  else if byte land 0b1110_0000 = 0b1100_0000 then index + 2
  else if byte land 0b1111_0000 = 0b1110_0000 then index + 3
  else if byte land 0b1111_1000 = 0b1111_0000 then index + 4
  else index + 1

let next_escape_index s index =
  let rec loop current =
    if current >= String.length s then
      current
    else
      let ch = Char.code (String.get s current) in
      if ch >= 0x40 && ch <= 0x7e then current + 1 else loop (current + 1)
  in
  loop (index + 2)

let visible_width s =
  let rec loop index width =
    if index >= String.length s then
      width
    else if index + 1 < String.length s && String.get s index = '\027'
            && String.get s (index + 1) = '['
    then
      loop (next_escape_index s index) width
    else
      loop (next_utf8_index s index) (width + 1)
  in
  loop 0 0

let fit_visible width s =
  if width <= 0 then ""
  else
    let buffer = Buffer.create (String.length s) in
    let rec loop index visible used_ansi =
      if index >= String.length s || visible >= width then (
        if used_ansi then Buffer.add_string buffer reset;
        Buffer.contents buffer)
      else if index + 1 < String.length s && String.get s index = '\027'
              && String.get s (index + 1) = '['
      then
        let next = next_escape_index s index in
        Buffer.add_substring buffer s index (next - index);
        loop next visible true
      else
        let next = next_utf8_index s index in
        Buffer.add_substring buffer s index (next - index);
        loop next (visible + 1) used_ansi
    in
    loop 0 0 false

let pad_right width text =
  let visible = visible_width text in
  if visible >= width then
    fit_visible width text
  else
    text ^ repeat " " (width - visible)

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
  Printf.sprintf "%.2f %s" (bps /. divisor) unit_name

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
  Printf.sprintf "%.2f %s" (float_of_int bytes /. divisor) unit_name

let maybe_ms = function
  | None -> "n/a"
  | Some value -> Printf.sprintf "%.2f ms" value

let maybe_bps = function
  | None -> "n/a"
  | Some value -> human_bps value

let timestamp_string timestamp =
  let tm = Unix.localtime timestamp in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min
    tm.tm_sec

let server_string server =
  let parts =
    List.filter_map Fun.id
      [
        server.colo;
        server.city;
        server.country;
        Option.map (fun asn -> "ASN " ^ asn) server.asn;
      ]
  in
  match parts with
  | [] -> "unknown"
  | _ -> String.concat " / " parts

let elapsed_string started_at =
  Printf.sprintf "%.1fs" (Unix.gettimeofday () -. started_at)

let color_for_bandwidth_mbps = function
  | value when value < 5. -> 196
  | value when value < 25. -> 214
  | value when value < 100. -> 118
  | value when value < 500. -> 45
  | _ -> 201

let color_for_latency_ms = function
  | value when value < 30. -> 118
  | value when value < 80. -> 190
  | value when value < 150. -> 214
  | value when value < 300. -> 208
  | _ -> 196

let metric_color kind value =
  match kind with
  | Bandwidth -> color_for_bandwidth_mbps value
  | Latency -> color_for_latency_ms value

let scale_position kind value width =
  if width <= 1 then
    0
  else
    let ratio =
      match kind with
      | Bandwidth ->
          let mbps = max 0. value in
          log10 (1. +. mbps) /. log10 1001.
      | Latency ->
          let ms = max 0. value in
          min 1. (ms /. 500.)
    in
    int_of_float (Float.round (ratio *. float_of_int (width - 1)))

let zone_color kind index width =
  let ratio =
    if width <= 1 then 0. else float_of_int index /. float_of_int (width - 1)
  in
  match kind with
  | Bandwidth ->
      if ratio < 0.2 then 196
      else if ratio < 0.4 then 208
      else if ratio < 0.65 then 190
      else if ratio < 0.85 then 118
      else 45
  | Latency ->
      if ratio < 0.2 then 118
      else if ratio < 0.4 then 190
      else if ratio < 0.65 then 214
      else if ratio < 0.85 then 208
      else 196

let heat_bar ~ansi kind width value_opt =
  match value_opt with
  | None -> dim_text ~ansi (repeat "·" width)
  | Some value ->
      let marker = scale_position kind value width in
      let cells =
        List.init width (fun index ->
            let glyph = if index = marker then "●" else "■" in
            let color =
              if index = marker then 15 else zone_color kind index width
            in
            colorize ~ansi color glyph)
      in
      String.concat "" cells

let sparkline_chars =
  [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

let sparkline ~width values =
  match values with
  | [] -> repeat "·" width
  | _ ->
      let data =
        if List.length values <= width then values
        else
          let values = Array.of_list values in
          List.init width (fun index ->
              let pos =
                int_of_float
                  (float_of_int index
                  *. float_of_int (Array.length values - 1)
                  /. float_of_int (width - 1))
              in
              values.(pos))
      in
      let min_v =
        List.fold_left min infinity data
      in
      let max_v =
        List.fold_left max neg_infinity data
      in
      let range =
        if max_v -. min_v < 0.000_001 then 1. else max_v -. min_v
      in
      let chars =
        List.map
          (fun value ->
            let ratio = (value -. min_v) /. range in
            let index =
              min 7
                (int_of_float
                   (Float.round (ratio *. float_of_int (Array.length sparkline_chars - 1))))
            in
            sparkline_chars.(index))
          data
      in
      let result = String.concat "" chars in
      if List.length data >= width then result else pad_right width result

let latest_sample_bps direction =
  match List.rev direction.samples with
  | [] -> None
  | sample :: _ -> Some sample.bps

let metric_text ~ansi kind value_opt =
  match value_opt with
  | None -> dim_text ~ansi "n/a"
  | Some value ->
      let text =
        match kind with
        | Bandwidth -> human_bps (value *. 1_000_000.)
        | Latency -> Printf.sprintf "%.2f ms" value
      in
      colorize ~ansi (metric_color kind value) text

let option_map default f = function
  | None -> default
  | Some value -> f value

let metric_row ~ansi ~label ~kind value_opt =
  let numeric =
    match kind, value_opt with
    | Bandwidth, Some value -> Some value
    | Latency, Some value -> Some value
    | _, None -> None
  in
  let text =
    match kind with
    | Bandwidth ->
        metric_text ~ansi Bandwidth
          (Option.map (fun value -> value /. 1_000_000.) value_opt)
    | Latency -> metric_text ~ansi Latency value_opt
  in
  let line =
    pad_right 12 label ^ " " ^ pad_right 14 text ^ " "
    ^ heat_bar ~ansi kind 20 numeric
  in
  line

let row ~label ~value =
  pad_right 12 label ^ " " ^ value

let panel ~width ~title lines =
  let inner = max 1 (width - 2) in
  let title_text = " " ^ title ^ " " in
  let title_width = visible_width title_text in
  let remaining = max 0 (inner - title_width) in
  let left = remaining / 2 in
  let right = remaining - left in
  let top = "┌" ^ repeat "─" left ^ title_text ^ repeat "─" right ^ "┐" in
  let body =
    List.map
      (fun line -> "│" ^ pad_right inner line ^ "│")
      lines
  in
  let bottom = "└" ^ repeat "─" inner ^ "┘" in
  top :: body @ [ bottom ]

let hstack left right =
  let blank_for lines =
    match lines with
    | [] -> ""
    | line :: _ -> repeat " " (visible_width line)
  in
  let left_blank = blank_for left in
  let right_blank = blank_for right in
  let max_len = max (List.length left) (List.length right) in
  let line_or_blank lines blank index =
    match List.nth_opt lines index with
    | Some line -> line
    | None -> blank
  in
  List.init max_len (fun index ->
      line_or_blank left left_blank index ^ " "
      ^ line_or_blank right right_blank index)

let metric_series_of_history metric entries =
  entries
  |> List.filter_map metric

let median values =
  Stats.percentile values 0.5

let delta_percent ~higher_is_better current baseline =
  match current, baseline with
  | Some current, Some baseline when baseline <> 0. ->
      let delta = ((current -. baseline) /. baseline) *. 100. in
      let good = if higher_is_better then delta >= 0. else delta <= 0. in
      Some (delta, good)
  | _ -> None

let delta_text ~ansi ~higher_is_better current baseline =
  match delta_percent ~higher_is_better current baseline with
  | None -> dim_text ~ansi "n/a"
  | Some (delta, good) ->
      let color = if good then 118 else 196 in
      let prefix = if delta >= 0. then "+" else "" in
      colorize ~ansi color (Printf.sprintf "%s%.1f%%" prefix delta)

let history_summary_lines ~ansi ~current_latency ~current_download ~current_upload
    (history : History.entry list) =
  let download_values =
    metric_series_of_history
      (fun (entry : History.entry) -> entry.download_bps)
      history
  in
  let upload_values =
    metric_series_of_history
      (fun (entry : History.entry) -> entry.upload_bps)
      history
  in
  let latency_values =
    metric_series_of_history
      (fun (entry : History.entry) -> entry.idle_latency_ms)
      history
  in
  let prev history_metric =
    history |> List.rev |> List.find_map history_metric
  in
  [
    row ~label:"DL trend"
      ~value:
        (sparkline ~width:18
           (List.map (fun value -> value /. 1_000_000.) download_values)
        ^ "  cur="
        ^ maybe_bps current_download);
    row ~label:"DL vs prev"
      ~value:
        (delta_text ~ansi ~higher_is_better:true current_download
           (prev (fun entry -> entry.download_bps))
        ^ "  median="
        ^ maybe_bps (median download_values));
    row ~label:"UL trend"
      ~value:
        (sparkline ~width:18
           (List.map (fun value -> value /. 1_000_000.) upload_values)
        ^ "  cur="
        ^ maybe_bps current_upload);
    row ~label:"UL vs prev"
      ~value:
        (delta_text ~ansi ~higher_is_better:true current_upload
           (prev (fun entry -> entry.upload_bps))
        ^ "  median="
        ^ maybe_bps (median upload_values));
    row ~label:"RTT trend"
      ~value:(sparkline ~width:18 latency_values ^ "  cur=" ^ maybe_ms current_latency);
    row ~label:"RTT vs prev"
      ~value:
        (delta_text ~ansi ~higher_is_better:false current_latency
           (prev (fun entry -> entry.idle_latency_ms))
        ^ "  median="
        ^ maybe_ms (median latency_values));
  ]

let history_table_lines ~ansi history =
  let recent =
    match history with
    | [] -> []
    | _ ->
        let total = List.length history in
        let drop = max 0 (total - 6) in
        let rec skip remaining = function
          | [] -> []
          | values when remaining <= 0 -> values
          | _ :: rest -> skip (remaining - 1) rest
        in
        skip drop history
  in
  if recent = [] then
    [ dim_text ~ansi "No saved runs yet." ]
  else
    recent
    |> List.rev
    |> List.map (fun (entry : History.entry) ->
           let server = Option.value entry.server.colo ~default:"?" in
           row
             ~label:
               (let tm = Unix.localtime entry.recorded_at in
                Printf.sprintf "%02d-%02d %02d:%02d" (tm.tm_mon + 1) tm.tm_mday
                  tm.tm_hour tm.tm_min)
             ~value:
               (pad_right 5 server ^ " DL " ^ maybe_bps entry.download_bps ^ "  UL "
              ^ maybe_bps entry.upload_bps ^ "  RTT " ^ maybe_ms entry.idle_latency_ms))

let phase_lines ~ansi (progress : progress) =
  let phase_text, detail =
    match progress.phase with
    | Fetching_meta -> ("Metadata lookup", dim_text ~ansi "resolving server info")
    | Idle_latency { completed; total } ->
        ( "Idle latency",
          Printf.sprintf "%d/%d samples  %s" completed total
            (heat_bar ~ansi Latency 16 (Some (float_of_int completed /. float_of_int (max 1 total) *. 500.))) )
    | Downloading { set_index; total_sets; request_bytes; completed; total } ->
        ( "Download",
          Printf.sprintf "set %d/%d  %s  %d/%d req"
            set_index total_sets (human_bytes request_bytes) completed total )
    | Uploading { set_index; total_sets; request_bytes; completed; total } ->
        ( "Upload",
          Printf.sprintf "set %d/%d  %s  %d/%d req"
            set_index total_sets (human_bytes request_bytes) completed total )
    | Finished -> ("Finished", "measurement complete")
  in
  [
    row ~label:"Phase" ~value:(bold_text ~ansi phase_text);
    row ~label:"Detail" ~value:detail;
    row ~label:"Elapsed" ~value:(elapsed_string progress.started_at);
    row ~label:"Server" ~value:(server_string progress.server);
    row ~label:"Preset" ~value:(Model.preset_to_string progress.preset);
  ]

let current_metric_lines ~ansi (progress : progress) =
  let idle_latency = Stats.percentile progress.idle_points 0.5 in
  let idle_jitter = Stats.jitter progress.idle_points in
  let download = Option.bind progress.download (fun direction -> direction.bandwidth_bps) in
  let upload = Option.bind progress.upload (fun direction -> direction.bandwidth_bps) in
  [
    metric_row ~ansi ~label:"Idle RTT" ~kind:Latency idle_latency;
    metric_row ~ansi ~label:"Idle jitter" ~kind:Latency idle_jitter;
    metric_row ~ansi ~label:"Download" ~kind:Bandwidth download;
    metric_row ~ansi ~label:"Upload" ~kind:Bandwidth upload;
  ]

let curves_lines ~ansi (progress : progress) =
  let idle_curve = sparkline ~width:28 progress.idle_points in
  let download_curve =
    progress.download
    |> option_map [] (fun direction ->
           List.map (fun sample -> sample.bps /. 1_000_000.) direction.samples)
    |> sparkline ~width:28
  in
  let upload_curve =
    progress.upload
    |> option_map [] (fun direction ->
           List.map (fun sample -> sample.bps /. 1_000_000.) direction.samples)
    |> sparkline ~width:28
  in
  [
    row ~label:"Idle RTT" ~value:idle_curve;
    row ~label:"Download" ~value:download_curve;
    row ~label:"Upload" ~value:upload_curve;
    row ~label:"Samples"
      ~value:
        (Printf.sprintf "idle=%d dl=%d ul=%d" (List.length progress.idle_points)
           (progress.download |> option_map 0 (fun direction -> List.length direction.samples))
           (progress.upload |> option_map 0 (fun direction -> List.length direction.samples)));
  ]

let terminal_columns () =
  match Terminal_size.get_columns () with
  | Some cols when cols >= 80 -> cols
  | _ -> 120

let assemble_dashboard ~title ~left_title ~left_lines ~right_title ~right_lines
    ~history_title ~history_lines ~footer_lines =
  let width = terminal_columns () in
  let inner = max 80 width in
  let col_width = (inner - 1) / 2 in
  let left = panel ~width:col_width ~title:left_title left_lines in
  let right = panel ~width:(inner - col_width - 1) ~title:right_title right_lines in
  let footer = panel ~width:inner ~title:history_title history_lines in
  let banner =
    panel ~width:inner ~title
      (List.map (fun line -> line) footer_lines)
  in
  String.concat "\n" (banner @ hstack left right @ footer)

let render_live ~ansi ~history (progress : progress) =
  let current_latency = Stats.percentile progress.idle_points 0.5 in
  let current_download =
    Option.bind progress.download (fun direction -> direction.bandwidth_bps)
  in
  let current_upload =
    Option.bind progress.upload (fun direction -> direction.bandwidth_bps)
  in
  assemble_dashboard ~title:" ohspeed TUI "
    ~left_title:" Session "
    ~left_lines:(phase_lines ~ansi progress @ [ "" ] @ current_metric_lines ~ansi progress)
    ~right_title:" Live Curves "
    ~right_lines:(curves_lines ~ansi progress @ [ "" ] @ history_table_lines ~ansi history)
    ~history_title:" History Compare "
    ~history_lines:
      (history_summary_lines ~ansi ~current_latency ~current_download
         ~current_upload history)
    ~footer_lines:
      [
        row ~label:"Mode" ~value:(bold_text ~ansi "live dashboard");
        row ~label:"Legend"
          ~value:
            ((colorize ~ansi 118 "good") ^ "  "
            ^ (colorize ~ansi 214 "watch") ^ "  "
            ^ (colorize ~ansi 196 "poor"));
      ]

let render_history ~ansi ~history =
  let current_latency =
    List.rev history
    |> List.find_map (fun (entry : History.entry) -> entry.idle_latency_ms)
  in
  let current_download =
    List.rev history
    |> List.find_map (fun (entry : History.entry) -> entry.download_bps)
  in
  let current_upload =
    List.rev history
    |> List.find_map (fun (entry : History.entry) -> entry.upload_bps)
  in
  assemble_dashboard ~title:" ohspeed History "
    ~left_title:" Recent Runs "
    ~left_lines:(history_table_lines ~ansi history)
    ~right_title:" Trend Curves "
    ~right_lines:
      [
        row ~label:"Latency"
          ~value:
            (sparkline ~width:36
               (history
               |> List.filter_map (fun (entry : History.entry) ->
                      entry.idle_latency_ms)));
        row ~label:"Download"
          ~value:
            (sparkline ~width:36
               (history
               |> List.filter_map (fun (entry : History.entry) ->
                      Option.map (fun value -> value /. 1_000_000.) entry.download_bps)));
        row ~label:"Upload"
          ~value:
            (sparkline ~width:36
               (history
               |> List.filter_map (fun (entry : History.entry) ->
                      Option.map (fun value -> value /. 1_000_000.) entry.upload_bps)));
      ]
    ~history_title:" Aggregate Compare "
    ~history_lines:
      (history_summary_lines ~ansi ~current_latency ~current_download
         ~current_upload history)
    ~footer_lines:
      [
        row ~label:"Saved file" ~value:(History.file_path ());
        row ~label:"Runs" ~value:(string_of_int (List.length history));
      ]

let create_session out = { out; started = false; last_frame = None }

let start session =
  if not session.started then (
    output_string session.out (esc "?1049h" ^ esc "?25l" ^ esc "H");
    flush session.out;
    session.started <- true)

let render session frame =
  if Some frame <> session.last_frame then (
    output_string session.out (esc "H");
    output_string session.out frame;
    output_string session.out (esc "J");
    flush session.out;
    session.last_frame <- Some frame)

let stop session =
  if session.started then (
    output_string session.out (esc "?25h" ^ esc "?1049l");
    flush session.out;
    session.started <- false;
    session.last_frame <- None)
