open Model

type session = {
  out : out_channel;
  mutable started : bool;
  mutable last_frame : string option;
}

type interactive_settings = {
  enable_download : bool;
  enable_upload : bool;
  save_history : bool;
  history_limit : int;
}

type metric_kind =
  | Bandwidth
  | Latency

let esc code = "\027[" ^ code
let reset = esc "0m"
let bold = esc "1m"
let dim = esc "2m"

let color256 code text = esc (Printf.sprintf "38;5;%dm" code) ^ text ^ reset
let colorize ~ansi code text = if ansi then color256 code text else text
let dim_text ~ansi text = if ansi then dim ^ text ^ reset else text
let bold_text ~ansi text = if ansi then bold ^ text ^ reset else text

let chip ~ansi ~fg ~bg text =
  if ansi then
    esc (Printf.sprintf "38;5;%d;48;5;%dm" fg bg) ^ " " ^ text ^ " " ^ reset
  else
    "[" ^ text ^ "]"

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

let truncate_visible width text =
  if width <= 0 then ""
  else if visible_width text <= width then
    text
  else if width = 1 then
    "…"
  else
    fit_visible (width - 1) text ^ "…"

let pad_right width text =
  let visible = visible_width text in
  if visible >= width then
    truncate_visible width text
  else
    text ^ repeat " " (width - visible)

let clamp low high value =
  max low (min high value)

let row_value_width width = max 8 (width - 16)
let curve_width width = clamp 10 42 (row_value_width width)
let summary_curve_width width = clamp 8 22 (row_value_width width / 2)

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

let maybe_bps_short = function
  | None -> "n/a"
  | Some value when value >= 1_000_000_000. ->
      Printf.sprintf "%.2fG" (value /. 1_000_000_000.)
  | Some value when value >= 1_000_000. -> Printf.sprintf "%.2fM" (value /. 1_000_000.)
  | Some value when value >= 1_000. -> Printf.sprintf "%.0fK" (value /. 1_000.)
  | Some value -> Printf.sprintf "%.0f" value

let maybe_ms_short = function
  | None -> "n/a"
  | Some value when value >= 100. -> Printf.sprintf "%.0fms" value
  | Some value -> Printf.sprintf "%.2fms" value

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

let latest_history_entry (history : History.entry list) =
  match List.rev history with
  | latest :: _ -> Some latest
  | [] -> None

let latest_history_metrics (history : History.entry list) =
  let latest = latest_history_entry history in
  let current_latency =
    Option.bind latest (fun (entry : History.entry) -> entry.idle_latency_ms)
  in
  let current_download =
    Option.bind latest (fun (entry : History.entry) -> entry.download_bps)
  in
  let current_upload =
    Option.bind latest (fun (entry : History.entry) -> entry.upload_bps)
  in
  (current_latency, current_download, current_upload)

let home_entries =
  [|
    (45, "Quick burst", "shortest low-latency probe", "1");
    (118, "Balanced sweep", "default profile", "2");
    (201, "Full saturation", "longest run, push the line", "3");
    (214, "History", "browse saved runs", "H");
    (111, "Settings", "toggles and history window", "S");
    (240, "Quit", "restore the terminal and exit", "Q");
  |]

let render_home_menu_lines ~ansi ~selected =
  home_entries
  |> Array.to_list
  |> List.mapi (fun index (accent, title, _subtitle, hotkey) ->
         let active = index = selected in
         let marker =
           if active then colorize ~ansi accent "▶" else " "
         in
         let hotkey_chip =
           if active then chip ~ansi ~fg:16 ~bg:accent hotkey
           else chip ~ansi ~fg:252 ~bg:238 hotkey
         in
         let title_text =
           if active then bold_text ~ansi title else title
         in
         marker ^ " " ^ hotkey_chip ^ " " ^ title_text)

let focus_lines ~ansi ~selected ~(history : History.entry list) ~settings =
  let latest = latest_history_entry history in
  let latest_server =
    match latest with
    | None -> dim_text ~ansi "no run yet"
    | Some entry -> server_string entry.server
  in
  match selected with
  | 0 | 1 | 2 ->
      let preset =
        match selected with
        | 0 -> Model.Quick
        | 1 -> Balanced
        | _ -> Full
      in
      let plan = Model.plan_of_preset preset in
      [
        row ~label:"Preset"
          ~value:(bold_text ~ansi (Model.preset_to_string preset));
        row ~label:"Latency"
          ~value:(Printf.sprintf "%d samples" plan.latency_samples);
        row ~label:"Download"
          ~value:
            (Printf.sprintf "%d lanes × %d stages" plan.download_parallel
               (List.length plan.download_sizes));
        row ~label:"Upload"
          ~value:
            (Printf.sprintf "%d lanes × %d stages" plan.upload_parallel
               (List.length plan.upload_sizes));
        row ~label:"Timeout"
          ~value:(Printf.sprintf "%.0fs per request" plan.request_timeout_s);
        "";
        row ~label:"Server" ~value:latest_server;
        row ~label:"Runs saved" ~value:(string_of_int (List.length history));
      ]
  | 3 ->
      [
        row ~label:"Saved runs" ~value:(string_of_int (List.length history));
        row ~label:"Window"
          ~value:(Printf.sprintf "%d runs" settings.history_limit);
        row ~label:"File" ~value:(History.file_path ());
        "";
        row ~label:"Latest" ~value:latest_server;
        dim_text ~ansi "Press Enter to browse history.";
      ]
  | 4 ->
      [
        row ~label:"Download"
          ~value:(if settings.enable_download then "on" else "off");
        row ~label:"Upload"
          ~value:(if settings.enable_upload then "on" else "off");
        row ~label:"Autosave"
          ~value:(if settings.save_history then "on" else "off");
        row ~label:"Window"
          ~value:(Printf.sprintf "%d runs" settings.history_limit);
        "";
        dim_text ~ansi "Press Enter to tune runtime switches.";
      ]
  | _ ->
      [
        dim_text ~ansi "Restore the terminal and exit.";
        "";
        row ~label:"Runs saved" ~value:(string_of_int (List.length history));
      ]

let settings_entries ~settings =
  [|
    ("Download stage", if settings.enable_download then "on" else "off");
    ("Upload stage", if settings.enable_upload then "on" else "off");
    ("Auto-save runs", if settings.save_history then "on" else "off");
    ("History window", Printf.sprintf "%d runs" settings.history_limit);
    ("Back", "");
  |]

let render_settings_menu_lines ~ansi ~settings ~selected =
  settings_entries ~settings
  |> Array.to_list
  |> List.mapi (fun index (title, value) ->
         let active = index = selected in
         let marker =
           if active then colorize ~ansi 111 "▶" else " "
         in
         let title_text =
           if active then bold_text ~ansi title else title
         in
         let value_text =
           if index = 4 then ""
           else if active then chip ~ansi ~fg:16 ~bg:111 value
           else dim_text ~ansi value
         in
         marker ^ " " ^ pad_right 18 title_text ^ " " ^ value_text)

let settings_detail_lines ~ansi ~selected ~settings =
  match selected with
  | 0 ->
      [
        row ~label:"Download"
          ~value:(if settings.enable_download then "on" else "off");
        "";
        dim_text ~ansi "Controls the concurrent GET stage.";
        dim_text ~ansi "Disable when you only care about upload.";
      ]
  | 1 ->
      [
        row ~label:"Upload"
          ~value:(if settings.enable_upload then "on" else "off");
        "";
        dim_text ~ansi "Controls the concurrent POST stage.";
        dim_text ~ansi "Disable for download-only checks.";
      ]
  | 2 ->
      [
        row ~label:"Autosave"
          ~value:(if settings.save_history then "on" else "off");
        row ~label:"File" ~value:(History.file_path ());
        "";
        dim_text ~ansi "Saved runs feed comparison panels.";
      ]
  | 3 ->
      [
        row ~label:"Window"
          ~value:(Printf.sprintf "%d runs" settings.history_limit);
        "";
        dim_text ~ansi "How many past runs fill the panels.";
        dim_text ~ansi "Use ←/→ to cycle the window size.";
      ]
  | _ ->
      [
        dim_text ~ansi "Return to the home screen.";
        dim_text ~ansi "Press Enter, Esc, or B.";
      ]

let report_curve_line width direction =
  match direction with
  | None -> repeat "·" width
  | Some direction ->
      direction.samples
      |> List.map (fun sample -> sample.bps /. 1_000_000.)
      |> sparkline ~width

let result_left_lines ~ansi (report : Model.report) =
  [
    row ~label:"Preset" ~value:(Model.preset_to_string report.preset);
    row ~label:"Server" ~value:(server_string report.server);
    row ~label:"Stamp" ~value:(timestamp_string report.generated_at);
    "";
    metric_row ~ansi ~label:"Idle RTT" ~kind:Latency report.idle_latency_ms;
    metric_row ~ansi ~label:"Idle jitter" ~kind:Latency report.idle_jitter_ms;
    metric_row ~ansi ~label:"Download" ~kind:Bandwidth
      (Option.bind report.download (fun direction -> direction.bandwidth_bps));
    metric_row ~ansi ~label:"Upload" ~kind:Bandwidth
      (Option.bind report.upload (fun direction -> direction.bandwidth_bps));
  ]

let result_right_lines ~ansi ~width (report : Model.report) =
  let spark_width = curve_width width in
  [
    row ~label:"Idle curve" ~value:(sparkline ~width:spark_width report.idle_points);
    row ~label:"Download" ~value:(report_curve_line spark_width report.download);
    row ~label:"Upload" ~value:(report_curve_line spark_width report.upload);
    row ~label:"DL chunk"
      ~value:
        (match report.download with
        | Some direction -> (
            match direction.selected_request_bytes with
            | Some bytes -> human_bytes bytes
            | None -> "n/a")
        | None -> "n/a");
    row ~label:"UL chunk"
      ~value:
        (match report.upload with
        | Some direction -> (
            match direction.selected_request_bytes with
            | Some bytes -> human_bytes bytes
            | None -> "n/a")
        | None -> "n/a");
    row ~label:"DL loaded"
      ~value:
        (match report.download with
        | Some direction -> maybe_ms direction.loaded_latency_ms
        | None -> "n/a");
    row ~label:"UL loaded"
      ~value:
        (match report.upload with
        | Some direction -> maybe_ms direction.loaded_latency_ms
        | None -> "n/a");
  ]

type border_style = {
  tl : string;
  tr : string;
  bl : string;
  br : string;
  h : string;
  v : string;
}

let light_border =
  { tl = "┌"; tr = "┐"; bl = "└"; br = "┘"; h = "─"; v = "│" }

let heavy_border =
  { tl = "┏"; tr = "┓"; bl = "┗"; br = "┛"; h = "━"; v = "┃" }

let panel ?(border = light_border) ?(title_align = `Left) ~width ~title lines =
  let inner = max 1 (width - 2) in
  let title_text = " " ^ title ^ " " in
  let title_width = visible_width title_text in
  let remaining = max 0 (inner - title_width) in
  let left, right =
    match title_align with
    | `Center ->
        let left = remaining / 2 in
        (left, remaining - left)
    | `Left ->
        let left = min 2 remaining in
        (left, remaining - left)
  in
  let top =
    border.tl ^ repeat border.h left ^ title_text ^ repeat border.h right
    ^ border.tr
  in
  let body =
    List.map
      (fun line -> border.v ^ pad_right inner line ^ border.v)
      lines
  in
  let bottom = border.bl ^ repeat border.h inner ^ border.br in
  top :: body @ [ bottom ]

let panel_width lines =
  match lines with
  | [] -> 0
  | line :: _ -> visible_width line

let hstack_many ?(gap = 1) panels =
  let widths = List.map panel_width panels in
  let blanks = List.map (fun width -> repeat " " width) widths in
  let height =
    List.fold_left (fun acc lines -> max acc (List.length lines)) 0 panels
  in
  let line_or_blank lines blank index =
    match List.nth_opt lines index with
    | Some line -> line
    | None -> blank
  in
  List.init height (fun index ->
      panels
      |> List.mapi (fun panel_index lines ->
             line_or_blank lines (List.nth blanks panel_index) index)
      |> String.concat (repeat " " gap))

let split_widths ~width count =
  let gap = 1 in
  let usable = max count (width - ((count - 1) * gap)) in
  let base = usable / count in
  let remainder = usable mod count in
  List.init count (fun index -> base + if index < remainder then 1 else 0)

let join_blocks blocks =
  blocks
  |> List.filter (fun block -> block <> [])
  |> List.mapi (fun index block -> if index = 0 then block else "" :: block)
  |> List.flatten
  |> String.concat "\n"

let take_lines count lines =
  let rec loop remaining acc = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | line :: rest -> loop (remaining - 1) (line :: acc) rest
  in
  loop count [] lines

let key_hint ~ansi key label =
  chip ~ansi ~fg:16 ~bg:238 key ^ " " ^ dim_text ~ansi label

let shortcut_bar ~ansi items =
  items
  |> List.map (fun (key, label) -> key_hint ~ansi key label)
  |> String.concat "  "

let info_badge ~ansi ~accent label value =
  chip ~ansi ~fg:16 ~bg:accent label ^ " " ^ value

let spinner_frames = [| "◴"; "◷"; "◶"; "◵" |]

let spinner ~ansi ~accent tick =
  colorize ~ansi accent spinner_frames.(tick mod Array.length spinner_frames)

(* compact_header — a 1-2 line borderless header.
   Layout: left side is "ohspeed · section  headline", right side is meta items.
   If the line would overflow, meta items move to a second line.
   Returns a list of strings (blocks). *)
let compact_header ~ansi ~width ~accent ~section ~headline ~meta_items =
  let tag =
    colorize ~ansi accent (bold_text ~ansi ("ohspeed · " ^ section))
  in
  let head = bold_text ~ansi headline in
  let left = tag ^ "  " ^ head in
  let meta_text =
    meta_items
    |> List.filter (fun item -> item <> "")
    |> String.concat (dim_text ~ansi "  ·  ")
  in
  let left_w = visible_width left in
  let meta_w = visible_width meta_text in
  if meta_text = "" then [ left ]
  else if left_w + meta_w + 2 <= width then
    [ left ^ repeat " " (width - left_w - meta_w) ^ meta_text ]
  else
    [ left; dim_text ~ansi meta_text ]

let footer_line ~ansi shortcuts = shortcut_bar ~ansi shortcuts

let render_transition ~ansi ~animation_tick:_ ~accent ~label ~progress =
  let width =
    match Terminal_size.get_columns () with
    | Some cols when cols >= 80 -> cols
    | _ -> 120
  in
  let progress_text = Printf.sprintf "%.0f%%" (progress *. 100.) in
  let inner = max 12 (width - 2) in
  let filled =
    clamp 0 inner (int_of_float (Float.round (progress *. float_of_int inner)))
  in
  let bar =
    List.init inner (fun index ->
        if index < filled then colorize ~ansi accent "█"
        else dim_text ~ansi "░")
    |> String.concat ""
  in
  join_blocks
    [
      compact_header ~ansi ~width ~accent ~section:"transition"
        ~headline:("Opening " ^ label)
        ~meta_items:[ "progress " ^ progress_text ];
      [ bar ];
    ]

let metric_card ~ansi ~width ~accent ~label ~kind value_opt ~detail_lines =
  let numeric =
    match kind with
    | Bandwidth -> Option.map (fun value -> value /. 1_000_000.) value_opt
    | Latency -> value_opt
  in
  let value_line =
    match kind with
    | Bandwidth -> metric_text ~ansi Bandwidth numeric
    | Latency -> metric_text ~ansi Latency value_opt
  in
  panel ~border:heavy_border ~title_align:`Center ~width
    ~title:(colorize ~ansi accent label)
    ([ value_line; heat_bar ~ansi kind (max 12 (width - 6)) numeric ]
    @ detail_lines)

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

let history_summary_lines ~ansi ~width ~current_latency ~current_download
    ~current_upload
    (history : History.entry list) =
  let spark_width = summary_curve_width width in
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
        (sparkline ~width:spark_width
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
        (sparkline ~width:spark_width
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
      ~value:
        (sparkline ~width:spark_width latency_values ^ "  cur="
       ^ maybe_ms current_latency);
    row ~label:"RTT vs prev"
      ~value:
        (delta_text ~ansi ~higher_is_better:false current_latency
           (prev (fun entry -> entry.idle_latency_ms))
        ^ "  median="
        ^ maybe_ms (median latency_values));
  ]

let history_curves ~history =
  let latency_values =
    history
    |> List.filter_map (fun (entry : History.entry) -> entry.idle_latency_ms)
  in
  let download_values =
    history
    |> List.filter_map (fun (entry : History.entry) ->
           Option.map (fun value -> value /. 1_000_000.) entry.download_bps)
  in
  let upload_values =
    history
    |> List.filter_map (fun (entry : History.entry) ->
           Option.map (fun value -> value /. 1_000_000.) entry.upload_bps)
  in
  (latency_values, download_values, upload_values)

let signal_snapshot_lines ~ansi ~width ~history =
  let spark_width = curve_width width in
  let latest_latency, latest_download, latest_upload =
    latest_history_metrics history
  in
  let latency_curve, download_curve, upload_curve = history_curves ~history in
  [
    row ~label:"Latest RTT" ~value:(metric_text ~ansi Latency latest_latency);
    row ~label:"Latest DL"
      ~value:
        (metric_text ~ansi Bandwidth
           (Option.map (fun value -> value /. 1_000_000.) latest_download));
    row ~label:"Latest UL"
      ~value:
        (metric_text ~ansi Bandwidth
           (Option.map (fun value -> value /. 1_000_000.) latest_upload));
    "";
    row ~label:"RTT curve" ~value:(sparkline ~width:spark_width latency_curve);
    row ~label:"DL curve" ~value:(sparkline ~width:spark_width download_curve);
    row ~label:"UL curve" ~value:(sparkline ~width:spark_width upload_curve);
  ]

let history_table_lines ?(limit = 6) ~ansi ~width history =
  let recent =
    match history with
    | [] -> []
    | _ ->
        let total = List.length history in
        let drop = max 0 (total - limit) in
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
               (let compact =
                  Printf.sprintf "%-3s DL %s  UL %s  RTT %s" server
                    (maybe_bps_short entry.download_bps)
                    (maybe_bps_short entry.upload_bps)
                    (maybe_ms_short entry.idle_latency_ms)
                in
                let detailed =
                  pad_right 4 server ^ " DL " ^ maybe_bps entry.download_bps
                  ^ "  UL " ^ maybe_bps entry.upload_bps ^ "  RTT "
                  ^ maybe_ms entry.idle_latency_ms
                in
                if visible_width detailed <= row_value_width width then
                  detailed
                else
                  compact))

let phase_summary ~ansi = function
  | Fetching_meta ->
      (111, "Metadata lookup", "resolving server info")
  | Idle_latency { completed; total } ->
      ( 214,
        "Idle latency",
        Printf.sprintf "%d/%d samples  %s" completed total
          (heat_bar ~ansi Latency 16
             (Some
                (float_of_int completed /. float_of_int (max 1 total) *. 500.))) )
  | Downloading { set_index; total_sets; request_bytes; completed; total } ->
      ( 45,
        "Download",
        Printf.sprintf "set %d/%d  %s  %d/%d req" set_index total_sets
          (human_bytes request_bytes) completed total )
  | Uploading { set_index; total_sets; request_bytes; completed; total } ->
      ( 201,
        "Upload",
        Printf.sprintf "set %d/%d  %s  %d/%d req" set_index total_sets
          (human_bytes request_bytes) completed total )
  | Finished -> (118, "Finished", "measurement complete")

let phase_lines ~ansi (progress : progress) =
  let _, phase_text, detail = phase_summary ~ansi progress.phase in
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

let curves_lines ~ansi ~width (progress : progress) =
  let spark_width = curve_width width in
  let idle_curve = sparkline ~width:spark_width progress.idle_points in
  let download_curve =
    progress.download
    |> option_map [] (fun direction ->
           List.map (fun sample -> sample.bps /. 1_000_000.) direction.samples)
    |> sparkline ~width:spark_width
  in
  let upload_curve =
    progress.upload
    |> option_map [] (fun direction ->
           List.map (fun sample -> sample.bps /. 1_000_000.) direction.samples)
    |> sparkline ~width:spark_width
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
  | Some cols when cols > 0 -> cols
  | _ -> 100

let terminal_rows () =
  match Terminal_size.get_rows () with
  | Some rows when rows > 0 -> rows
  | _ -> 28

let dashboard_width () = max 48 (terminal_columns ())
let dashboard_height () = max 16 (terminal_rows ())
let compact_screen () = dashboard_height () <= 26

let pair_widths width =
  match split_widths ~width 2 with
  | [ left; right ] -> (left, right)
  | _ -> assert false

let triple_widths width =
  match split_widths ~width 3 with
  | [ first; second; third ] -> (first, second, third)
  | _ -> assert false

let history_trend_lines ~ansi ~width ~history =
  let spark_width = curve_width width in
  let latency_curve, download_curve, upload_curve = history_curves ~history in
  [
    row ~label:"Latency" ~value:(sparkline ~width:spark_width latency_curve);
    row ~label:"Download" ~value:(sparkline ~width:spark_width download_curve);
    row ~label:"Upload" ~value:(sparkline ~width:spark_width upload_curve);
    "";
    row ~label:"Median RTT" ~value:(maybe_ms (median latency_curve));
    row ~label:"Median DL"
      ~value:
        (maybe_bps
           (Option.map (fun value -> value *. 1_000_000.) (median download_curve)));
    row ~label:"Median UL"
      ~value:
        (maybe_bps
           (Option.map (fun value -> value *. 1_000_000.) (median upload_curve)));
  ]

let history_storage_lines ~ansi ~history ~history_limit =
  let latest_server =
    match latest_history_entry history with
    | None -> dim_text ~ansi "No saved run yet"
    | Some entry -> server_string entry.server
  in
  [
    row ~label:"Runs loaded" ~value:(string_of_int (List.length history));
    row ~label:"Window"
      ~value:(chip ~ansi ~fg:16 ~bg:214 (Printf.sprintf "%d runs" history_limit));
    row ~label:"Saved file" ~value:(History.file_path ());
    "";
    row ~label:"Latest" ~value:latest_server;
  ]

let history_before_report history (report : Model.report) =
  match List.rev history with
  | (latest : History.entry) :: rest
    when abs_float (latest.recorded_at -. report.generated_at) < 0.000_001 ->
      List.rev rest
  | _ -> history

let render_live ~ansi ~animation_tick ~history:_ (progress : progress) =
  let width = dashboard_width () in
  let accent, phase_text, detail = phase_summary ~ansi progress.phase in
  let current_latency = Stats.percentile progress.idle_points 0.5 in
  let current_download =
    Option.bind progress.download (fun direction -> direction.bandwidth_bps)
  in
  let current_upload =
    Option.bind progress.upload (fun direction -> direction.bandwidth_bps)
  in
  let header =
    compact_header ~ansi ~width ~accent ~section:"live"
      ~headline:(phase_text ^ " " ^ spinner ~ansi ~accent animation_tick)
      ~meta_items:
        [
          "server " ^ server_string progress.server;
          "preset " ^ Model.preset_to_string progress.preset;
          "elapsed " ^ elapsed_string progress.started_at;
        ]
  in
  let footer = [ footer_line ~ansi [ ("Ctrl+C", "cancel") ] ] in
  let status_lines =
    [
      row ~label:"Phase" ~value:(bold_text ~ansi phase_text);
      row ~label:"Detail" ~value:detail;
      "";
    ]
    @ current_metric_lines ~ansi progress
  in
  if compact_screen () then
    let body =
      if width >= 72 then
        let left, right = pair_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi accent "Status")
                (take_lines 9 status_lines);
              panel ~width:right
                ~title:(colorize ~ansi 45 "Curves")
                (curves_lines ~ansi ~width:right progress);
            ];
        ]
      else
        [
          panel ~width ~title:(colorize ~ansi accent "Status")
            (take_lines 9 status_lines);
          panel ~width ~title:(colorize ~ansi 45 "Curves")
            (curves_lines ~ansi ~width progress);
        ]
    in
    join_blocks ([ header ] @ body @ [ footer ])
  else
    let idle_card width =
      metric_card ~ansi ~width ~accent:214 ~label:"Idle RTT" ~kind:Latency
        current_latency
        ~detail_lines:
          [
            dim_text ~ansi
              ("jitter " ^ maybe_ms (Stats.jitter progress.idle_points));
            dim_text ~ansi
              (Printf.sprintf "%d idle probes"
                 (List.length progress.idle_points));
          ]
    in
    let download_card width =
      metric_card ~ansi ~width ~accent:45 ~label:"Download" ~kind:Bandwidth
        current_download
        ~detail_lines:
          [
            dim_text ~ansi
              ("latest "
              ^ maybe_bps
                  (Option.bind progress.download (fun direction ->
                       latest_sample_bps direction)));
            dim_text ~ansi
              ("loaded "
              ^ maybe_ms
                  (Option.bind progress.download (fun direction ->
                       direction.loaded_latency_ms)));
          ]
    in
    let upload_card width =
      metric_card ~ansi ~width ~accent:201 ~label:"Upload" ~kind:Bandwidth
        current_upload
        ~detail_lines:
          [
            dim_text ~ansi
              ("latest "
              ^ maybe_bps
                  (Option.bind progress.upload (fun direction ->
                       latest_sample_bps direction)));
            dim_text ~ansi
              ("loaded "
              ^ maybe_ms
                  (Option.bind progress.upload (fun direction ->
                       direction.loaded_latency_ms)));
          ]
    in
    let cards =
      if width >= 132 then
        let left, center, right = triple_widths width in
        [ hstack_many [ idle_card left; download_card center; upload_card right ] ]
      else if width >= 104 then
        let left, right = pair_widths width in
        [ hstack_many [ idle_card left; download_card right ]; upload_card width ]
      else
        [ idle_card width; download_card width; upload_card width ]
    in
    let main_row =
      if width >= 108 then
        let left, right = pair_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi accent "Status") status_lines;
              panel ~width:right
                ~title:(colorize ~ansi 45 "Curves")
                (curves_lines ~ansi ~width:right progress);
            ];
        ]
      else
        [
          panel ~width ~title:(colorize ~ansi accent "Status") status_lines;
          panel ~width ~title:(colorize ~ansi 45 "Curves")
            (curves_lines ~ansi ~width progress);
        ]
    in
    join_blocks ([ header ] @ cards @ main_row @ [ footer ])

let render_history_page ~ansi ~animation_tick:_ ~history ~history_limit
    ~scale_label ~section ~headline ~shortcuts =
  let width = dashboard_width () in
  let current_latency, current_download, current_upload =
    latest_history_metrics history
  in
  let header =
    compact_header ~ansi ~width ~accent:214 ~section ~headline
      ~meta_items:
        [
          Printf.sprintf "%d runs" (List.length history);
          "scale " ^ scale_label;
          Printf.sprintf "window %d" history_limit;
        ]
  in
  let footer = [ footer_line ~ansi shortcuts ] in
  if compact_screen () then
    let latest_server =
      match latest_history_entry history with
      | None -> dim_text ~ansi "no run yet"
      | Some entry -> server_string entry.server
    in
    let storage_lines =
      if width >= 84 then
        take_lines 7
          (history_storage_lines ~ansi ~history ~history_limit
          @ [ row ~label:"Scale" ~value:scale_label ]
          @ signal_snapshot_lines ~ansi ~width ~history)
      else
        [
          row ~label:"Runs" ~value:(string_of_int (List.length history));
          row ~label:"Scale" ~value:scale_label;
          row ~label:"Latest" ~value:latest_server;
          row ~label:"RTT" ~value:(maybe_ms current_latency);
          row ~label:"DL/UL"
            ~value:
              (maybe_bps current_download ^ " / " ^ maybe_bps current_upload);
        ]
    in
    let body =
      if width >= 84 then
        let left, right = pair_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi 214 "Runs")
                (history_table_lines ~limit:(min 5 history_limit) ~ansi
                   ~width:left history);
              panel ~width:right
                ~title:(colorize ~ansi 111 "Storage")
                storage_lines;
            ];
        ]
      else
        [
          panel ~width ~title:(colorize ~ansi 214 "Runs")
            (history_table_lines ~limit:(min 4 history_limit) ~ansi ~width
               history);
          panel ~width ~title:(colorize ~ansi 111 "Storage") storage_lines;
        ]
    in
    join_blocks ([ header ] @ body @ [ footer ])
  else
    let body =
      if width >= 132 then
        let left, center, right = triple_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi 214 "Runs")
                (history_table_lines ~limit:history_limit ~ansi ~width:left
                   history);
              panel ~width:center
                ~title:(colorize ~ansi 45 "Trends")
                (history_trend_lines ~ansi ~width:center ~history);
              panel ~width:right
                ~title:(colorize ~ansi 111 "Storage")
                (history_storage_lines ~ansi ~history ~history_limit
                @ [ row ~label:"Scale" ~value:scale_label; "" ]
                @ signal_snapshot_lines ~ansi ~width:right ~history);
            ];
        ]
      else if width >= 108 then
        let left, right = pair_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi 214 "Runs")
                (history_table_lines ~limit:history_limit ~ansi ~width:left
                   history);
              panel ~width:right
                ~title:(colorize ~ansi 45 "Trends")
                (history_trend_lines ~ansi ~width:right ~history);
            ];
          panel ~width ~title:(colorize ~ansi 111 "Storage")
            (history_storage_lines ~ansi ~history ~history_limit
            @ [ row ~label:"Scale" ~value:scale_label; "" ]
            @ signal_snapshot_lines ~ansi ~width ~history);
        ]
      else
        [
          panel ~width ~title:(colorize ~ansi 214 "Runs")
            (history_table_lines ~limit:history_limit ~ansi ~width history);
          panel ~width ~title:(colorize ~ansi 45 "Trends")
            (history_trend_lines ~ansi ~width ~history);
          panel ~width ~title:(colorize ~ansi 111 "Storage")
            (history_storage_lines ~ansi ~history ~history_limit
            @ [ row ~label:"Scale" ~value:scale_label; "" ]
            @ signal_snapshot_lines ~ansi ~width ~history);
        ]
    in
    let compare =
      panel ~width ~title:(colorize ~ansi 118 "Deltas")
        (history_summary_lines ~ansi ~width ~current_latency ~current_download
           ~current_upload history)
    in
    join_blocks ([ header ] @ body @ [ compare ] @ [ footer ])

let render_history ~ansi ~animation_tick ~history =
  render_history_page ~ansi ~animation_tick ~history
    ~history_limit:(max 1 (List.length history)) ~scale_label:"all"
    ~section:"history" ~headline:"Saved benchmark history"
    ~shortcuts:
      [
        ("↑↓", "browse");
        ("B", "back");
        ("Q", "quit");
      ]

let render_home ~ansi ~animation_tick:_ ~history ~settings ~selected =
  let width = dashboard_width () in
  let accent, title, _subtitle, _ = home_entries.(selected) in
  let header =
    compact_header ~ansi ~width ~accent ~section:"home" ~headline:title
      ~meta_items:
        [
          Printf.sprintf "%d runs saved" (List.length history);
          "dl " ^ (if settings.enable_download then "on" else "off");
          "ul " ^ (if settings.enable_upload then "on" else "off");
          "save " ^ (if settings.save_history then "on" else "off");
        ]
  in
  let footer =
    [
      footer_line ~ansi
        [
          ("↑↓", "move");
          ("Enter", "run");
          ("H", "history");
          ("S", "settings");
          ("Q", "quit");
        ];
    ]
  in
  let body =
    if width >= 108 then
      let left, right = pair_widths width in
      [
        hstack_many
          [
            panel ~width:left
              ~title:(colorize ~ansi accent "Menu")
              (render_home_menu_lines ~ansi ~selected);
            panel ~width:right
              ~title:(colorize ~ansi 45 "Plan")
              (focus_lines ~ansi ~selected ~history ~settings);
          ];
      ]
    else
      [
        panel ~width
          ~title:(colorize ~ansi accent "Menu")
          (render_home_menu_lines ~ansi ~selected);
        panel ~width
          ~title:(colorize ~ansi 45 "Plan")
          (focus_lines ~ansi ~selected ~history ~settings);
      ]
  in
  join_blocks ([ header ] @ body @ [ footer ])

let render_settings ~ansi ~animation_tick:_ ~history:_ ~settings ~selected =
  let width = dashboard_width () in
  let selected_title, _ = (settings_entries ~settings).(selected) in
  let header =
    compact_header ~ansi ~width ~accent:111 ~section:"settings"
      ~headline:selected_title
      ~meta_items:
        [
          "dl " ^ (if settings.enable_download then "on" else "off");
          "ul " ^ (if settings.enable_upload then "on" else "off");
          "save " ^ (if settings.save_history then "on" else "off");
          Printf.sprintf "window %d runs" settings.history_limit;
        ]
  in
  let footer =
    [
      footer_line ~ansi
        [
          ("↑↓", "move");
          ("←→", "change");
          ("Enter", "toggle");
          ("B", "back");
          ("Q", "quit");
        ];
    ]
  in
  let body =
    if width >= 108 then
      let left, right = pair_widths width in
      [
        hstack_many
          [
            panel ~width:left
              ~title:(colorize ~ansi 111 "Options")
              (render_settings_menu_lines ~ansi ~settings ~selected);
            panel ~width:right
              ~title:(colorize ~ansi 45 "Detail")
              (settings_detail_lines ~ansi ~selected ~settings);
          ];
      ]
    else
      [
        panel ~width
          ~title:(colorize ~ansi 111 "Options")
          (render_settings_menu_lines ~ansi ~settings ~selected);
        panel ~width
          ~title:(colorize ~ansi 45 "Detail")
          (settings_detail_lines ~ansi ~selected ~settings);
      ]
  in
  join_blocks ([ header ] @ body @ [ footer ])

let render_history_browser ~ansi ~animation_tick ~history ~settings
    ~scale_label =
  render_history_page ~ansi ~animation_tick ~history
    ~history_limit:settings.history_limit ~scale_label
    ~section:"history" ~headline:"Compare saved runs"
    ~shortcuts:
      [
        ("←→", "scale");
        ("1-4", "24h/7d/30d/all");
        ("B", "back");
        ("Q", "quit");
      ]

let render_result ~ansi ~animation_tick:_ ~history ~settings
    ~(report : Model.report) =
  let width = dashboard_width () in
  let comparison_history = history_before_report history report in
  let current_download =
    Option.bind report.download (fun direction -> direction.bandwidth_bps)
  in
  let current_upload =
    Option.bind report.upload (fun direction -> direction.bandwidth_bps)
  in
  let header =
    compact_header ~ansi ~width ~accent:118 ~section:"result"
      ~headline:"Run complete"
      ~meta_items:
        [
          Model.preset_to_string report.preset;
          server_string report.server;
          timestamp_string report.generated_at;
          (if settings.save_history then "saved" else "not saved");
        ]
  in
  let footer =
    [
      footer_line ~ansi
        [
          ("Enter", "rerun");
          ("H", "history");
          ("B", "home");
          ("Q", "quit");
        ];
    ]
  in
  if compact_screen () then
    let body =
      if width >= 84 then
        let left, right = pair_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi 118 "Summary")
                (take_lines 8 (result_left_lines ~ansi report));
              panel ~width:right
                ~title:(colorize ~ansi 45 "Curves")
                (take_lines 8 (result_right_lines ~ansi ~width:right report));
            ];
        ]
      else
        [
          panel ~width
            ~title:(colorize ~ansi 118 "Summary")
            (take_lines 8 (result_left_lines ~ansi report));
          panel ~width
            ~title:(colorize ~ansi 45 "Curves")
            (take_lines 8 (result_right_lines ~ansi ~width report));
        ]
    in
    join_blocks ([ header ] @ body @ [ footer ])
  else
    let idle_card width =
      metric_card ~ansi ~width ~accent:214 ~label:"Idle RTT" ~kind:Latency
        report.idle_latency_ms
        ~detail_lines:
          [
            dim_text ~ansi ("jitter " ^ maybe_ms report.idle_jitter_ms);
            dim_text ~ansi
              (Printf.sprintf "%d idle samples"
                 (List.length report.idle_points));
          ]
    in
    let download_card width =
      metric_card ~ansi ~width ~accent:45 ~label:"Download" ~kind:Bandwidth
        current_download
        ~detail_lines:
          [
            dim_text ~ansi
              ("chunk "
              ^
              match report.download with
              | Some direction -> (
                  match direction.selected_request_bytes with
                  | Some bytes -> human_bytes bytes
                  | None -> "n/a")
              | None -> "n/a");
            dim_text ~ansi
              ("loaded "
              ^
              match report.download with
              | Some direction -> maybe_ms direction.loaded_latency_ms
              | None -> "n/a");
          ]
    in
    let upload_card width =
      metric_card ~ansi ~width ~accent:201 ~label:"Upload" ~kind:Bandwidth
        current_upload
        ~detail_lines:
          [
            dim_text ~ansi
              ("chunk "
              ^
              match report.upload with
              | Some direction -> (
                  match direction.selected_request_bytes with
                  | Some bytes -> human_bytes bytes
                  | None -> "n/a")
              | None -> "n/a");
            dim_text ~ansi
              ("loaded "
              ^
              match report.upload with
              | Some direction -> maybe_ms direction.loaded_latency_ms
              | None -> "n/a");
          ]
    in
    let cards =
      if width >= 132 then
        let left, center, right = triple_widths width in
        [ hstack_many [ idle_card left; download_card center; upload_card right ] ]
      else if width >= 104 then
        let left, right = pair_widths width in
        [ hstack_many [ idle_card left; download_card right ]; upload_card width ]
      else
        [ idle_card width; download_card width; upload_card width ]
    in
    let body =
      if width >= 108 then
        let left, right = pair_widths width in
        [
          hstack_many
            [
              panel ~width:left
                ~title:(colorize ~ansi 118 "Summary")
                (result_left_lines ~ansi report);
              panel ~width:right
                ~title:(colorize ~ansi 45 "Curves")
                (result_right_lines ~ansi ~width:right report);
            ];
        ]
      else
        [
          panel ~width
            ~title:(colorize ~ansi 118 "Summary")
            (result_left_lines ~ansi report);
          panel ~width
            ~title:(colorize ~ansi 45 "Curves")
            (result_right_lines ~ansi ~width report);
        ]
    in
    let compare =
      panel ~width
        ~title:(colorize ~ansi 214 "vs history")
        (history_summary_lines ~ansi ~width
           ~current_latency:report.idle_latency_ms ~current_download
           ~current_upload comparison_history)
    in
    join_blocks ([ header ] @ cards @ body @ [ compare ] @ [ footer ])

let render_notice ~ansi ~animation_tick:_ ~title ~subtitle ~detail_lines
    ~footer_lines =
  let width = dashboard_width () in
  let header =
    compact_header ~ansi ~width ~accent:196 ~section:"alert" ~headline:title
      ~meta_items:(if subtitle = "" then [] else [ subtitle ])
  in
  let footer =
    [ footer_line ~ansi [ ("Enter", "home"); ("B", "back"); ("Q", "quit") ] ]
  in
  join_blocks
    [
      header;
      panel ~width ~title:(colorize ~ansi 196 "Detail")
        (detail_lines @ [ "" ] @ footer_lines);
      footer;
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
