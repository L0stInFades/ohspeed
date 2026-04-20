type preset =
  | Quick
  | Balanced
  | Full

val preset_to_string : preset -> string
val preset_of_string : string -> (preset, string) result

type output =
  | Text
  | Json

val output_to_string : output -> string
val output_of_string : string -> (output, string) result

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

val empty_server_meta : server_meta
val merge_server_meta : server_meta -> server_meta -> server_meta

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

val plan_of_preset : preset -> plan

val with_plan_overrides :
  ?latency_samples:int ->
  ?download_parallel:int ->
  ?upload_parallel:int ->
  ?download_sizes:int list ->
  ?upload_sizes:int list ->
  ?request_timeout_s:float ->
  plan ->
  plan

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

val parse_int_csv : name:string -> string -> (int list, string) result
