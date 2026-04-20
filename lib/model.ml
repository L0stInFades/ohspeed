type preset =
  | Quick
  | Balanced
  | Full

let preset_to_string = function
  | Quick -> "quick"
  | Balanced -> "balanced"
  | Full -> "full"

let preset_of_string value =
  match String.lowercase_ascii value with
  | "quick" -> Ok Quick
  | "balanced" -> Ok Balanced
  | "full" -> Ok Full
  | _ -> Error "preset must be one of: quick, balanced, full"

type output =
  | Text
  | Json

let output_to_string = function
  | Text -> "text"
  | Json -> "json"

let output_of_string value =
  match String.lowercase_ascii value with
  | "text" -> Ok Text
  | "json" -> Ok Json
  | _ -> Error "output must be one of: text, json"

type endpoints = {
  download_url : string;
  upload_url : string;
  meta_url : string option;
}

type latency_sample = {
  ping_ms : float;
  status_code : int;
  timestamp_s : float;
}

type bandwidth_sample = {
  request_bytes : int;
  transfer_bytes : int;
  duration_s : float;
  bps : float;
  ttfb_ms : float;
  status_code : int;
}

type bandwidth_set = {
  request_bytes : int;
  samples : bandwidth_sample list;
  side_latencies : latency_sample list;
  min_duration_s : float option;
}

type direction_result = {
  bandwidth_bps : float option;
  samples : bandwidth_sample list;
  qualified_samples : int;
  sets : bandwidth_set list;
  loaded_latency_ms : float option;
  loaded_jitter_ms : float option;
  loaded_points : float list;
  selected_request_bytes : int option;
}

type server_meta = {
  colo : string option;
  city : string option;
  country : string option;
  asn : string option;
  ip : string option;
}

let empty_server_meta =
  { colo = None; city = None; country = None; asn = None; ip = None }

let merge_option left right =
  match left with
  | Some _ -> left
  | None -> right

let merge_server_meta left right =
  {
    colo = merge_option left.colo right.colo;
    city = merge_option left.city right.city;
    country = merge_option left.country right.country;
    asn = merge_option left.asn right.asn;
    ip = merge_option left.ip right.ip;
  }

type plan = {
  latency_samples : int;
  latency_interval_s : float;
  latency_percentile : float;
  bandwidth_percentile : float;
  side_probe_interval_s : float;
  request_timeout_s : float;
  min_sample_duration_s : float;
  saturation_target_s : float;
  loaded_latency_min_s : float;
  loaded_max_points : int;
  download_parallel : int;
  upload_parallel : int;
  download_sizes : int list;
  upload_sizes : int list;
}

let plan_of_preset = function
  | Quick ->
      {
        latency_samples = 8;
        latency_interval_s = 0.12;
        latency_percentile = 0.5;
        bandwidth_percentile = 0.9;
        side_probe_interval_s = 0.35;
        request_timeout_s = 12.;
        min_sample_duration_s = 0.05;
        saturation_target_s = 0.18;
        loaded_latency_min_s = 0.15;
        loaded_max_points = 16;
        download_parallel = 4;
        upload_parallel = 3;
        download_sizes = [ 250_000; 1_000_000; 8_000_000 ];
        upload_sizes = [ 250_000; 1_000_000; 4_000_000 ];
      }
  | Balanced ->
      {
        latency_samples = 14;
        latency_interval_s = 0.15;
        latency_percentile = 0.5;
        bandwidth_percentile = 0.9;
        side_probe_interval_s = 0.4;
        request_timeout_s = 15.;
        min_sample_duration_s = 0.08;
        saturation_target_s = 0.25;
        loaded_latency_min_s = 0.2;
        loaded_max_points = 20;
        download_parallel = 4;
        upload_parallel = 4;
        download_sizes = [ 250_000; 1_000_000; 8_000_000; 24_000_000 ];
        upload_sizes = [ 250_000; 1_000_000; 4_000_000; 8_000_000 ];
      }
  | Full ->
      {
        latency_samples = 20;
        latency_interval_s = 0.18;
        latency_percentile = 0.5;
        bandwidth_percentile = 0.9;
        side_probe_interval_s = 0.45;
        request_timeout_s = 20.;
        min_sample_duration_s = 0.1;
        saturation_target_s = 0.35;
        loaded_latency_min_s = 0.25;
        loaded_max_points = 24;
        download_parallel = 6;
        upload_parallel = 5;
        download_sizes = [ 250_000; 1_000_000; 8_000_000; 24_000_000; 64_000_000 ];
        upload_sizes = [ 250_000; 1_000_000; 4_000_000; 8_000_000; 16_000_000 ];
      }

let with_plan_overrides ?latency_samples ?download_parallel ?upload_parallel
    ?download_sizes ?upload_sizes ?request_timeout_s plan =
  let choose override fallback =
    match override with
    | Some value -> value
    | None -> fallback
  in
  {
    plan with
    latency_samples = choose latency_samples plan.latency_samples;
    download_parallel = choose download_parallel plan.download_parallel;
    upload_parallel = choose upload_parallel plan.upload_parallel;
    download_sizes = choose download_sizes plan.download_sizes;
    upload_sizes = choose upload_sizes plan.upload_sizes;
    request_timeout_s = choose request_timeout_s plan.request_timeout_s;
  }

type report = {
  generated_at : float;
  preset : preset;
  server : server_meta;
  idle_latency_ms : float option;
  idle_jitter_ms : float option;
  idle_points : float list;
  download : direction_result option;
  upload : direction_result option;
}

type phase =
  | Fetching_meta
  | Idle_latency of {
      completed : int;
      total : int;
    }
  | Downloading of {
      set_index : int;
      total_sets : int;
      request_bytes : int;
      completed : int;
      total : int;
    }
  | Uploading of {
      set_index : int;
      total_sets : int;
      request_bytes : int;
      completed : int;
      total : int;
    }
  | Finished

type progress = {
  started_at : float;
  preset : preset;
  phase : phase;
  server : server_meta;
  idle_points : float list;
  download_enabled : bool;
  upload_enabled : bool;
  download : direction_result option;
  upload : direction_result option;
}

type run_config = {
  preset : preset;
  plan : plan;
  endpoints : endpoints;
  enable_download : bool;
  enable_upload : bool;
}

let parse_int_csv ~name csv =
  let trimmed = String.trim csv in
  if trimmed = "" then
    Error (Printf.sprintf "%s cannot be empty" name)
  else
    let parts = String.split_on_char ',' trimmed in
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | raw :: rest ->
          let item = String.trim raw in
          (match int_of_string_opt item with
          | Some value when value > 0 -> loop (value :: acc) rest
          | _ ->
              Error
                (Printf.sprintf "%s must contain only positive integers" name))
    in
    loop [] parts
