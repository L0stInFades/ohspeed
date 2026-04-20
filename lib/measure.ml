open Eio.Std

module Body = Cohttp_eio.Body
module Client = Cohttp_eio.Client
module Code = Cohttp.Code
module Header = Cohttp.Header
module Response = Cohttp.Response

open Model

type direction =
  | Download
  | Upload

let empty_bandwidth_set request_bytes : bandwidth_set =
  { request_bytes; samples = []; side_latencies = []; min_duration_s = None }

let update_min_duration current candidate =
  match current with
  | None -> Some candidate
  | Some current -> Some (Float.min current candidate)

let add_bandwidth_sample (set : bandwidth_set) sample : bandwidth_set =
  {
    set with
    samples = set.samples @ [ sample ];
    min_duration_s = update_min_duration set.min_duration_s sample.duration_s;
  }

let add_side_latency (set : bandwidth_set) sample : bandwidth_set =
  { set with side_latencies = set.side_latencies @ [ sample ] }

let payload_cache : (int, string) Hashtbl.t = Hashtbl.create 8
let rng_initialized = Atomic.make false

let is_success status =
  let code = Code.code_of_status status in
  code >= 200 && code < 300

let header_first headers keys = List.find_map (Header.get headers) keys

let server_of_headers headers =
  {
    colo = header_first headers [ "colo"; "cf-meta-colo" ];
    city = header_first headers [ "city"; "cf-meta-city" ];
    country = header_first headers [ "country"; "cf-meta-country" ];
    asn = header_first headers [ "asn"; "cf-meta-asn" ];
    ip = header_first headers [ "cf-meta-ip"; "ip" ];
  }

let add_cache_bust url =
  let nonce =
    Printf.sprintf "%016x%016x"
      (Int64.to_int (Int64.bits_of_float (Unix.gettimeofday ())))
      (Random.bits ())
  in
  let uri = Uri.of_string url in
  Uri.add_query_param' uri ("cache", nonce)

let download_uri endpoints bytes =
  let uri = add_cache_bust endpoints.download_url in
  Uri.add_query_param' uri ("bytes", string_of_int bytes)

let upload_uri endpoints = add_cache_bust endpoints.upload_url

let payload_of_size bytes =
  match Hashtbl.find_opt payload_cache bytes with
  | Some payload -> payload
  | None ->
      let payload = Bytes.make bytes 'a' |> Bytes.unsafe_to_string in
      Hashtbl.replace payload_cache bytes payload;
      payload

let json_string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Some value
  | `Int value -> Some (string_of_int value)
  | `Intlit value -> Some value
  | `Float value -> Some (string_of_float value)
  | _ -> None

let ensure_rng () =
  if Atomic.compare_and_set rng_initialized false true then
    Mirage_crypto_rng_unix.use_default ()

let https_connector () =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok authenticator -> authenticator
    | Error (`Msg msg) ->
        failwith
          ("ohspeed: failed to create system TLS authenticator: " ^ msg)
  in
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Ok tls_config -> tls_config
    | Error (`Msg msg) ->
        failwith ("ohspeed: failed to create TLS client config: " ^ msg)
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun host_name ->
             Domain_name.(host_exn (of_string_exn host_name)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let make_client env =
  ensure_rng ();
  Client.make ~https:(Some (https_connector ())) env#net

let drain_and_count body =
  let total = ref 0 in
  let buffer = Cstruct.create 65_536 in
  try
    while true do
      total := !total + Eio.Flow.single_read body buffer
    done;
    assert false
  with End_of_file -> !total

let fetch_server_meta ~clock ~client endpoints =
  match endpoints.meta_url with
  | None -> empty_server_meta
  | Some url -> (
      try
        Eio.Time.with_timeout_exn clock 5. (fun () ->
            Eio.Switch.run @@ fun sw ->
            let response, body = Client.get client ~sw (add_cache_bust url) in
            let headers = Response.headers response in
            let raw = Eio.Flow.read_all body in
            let json = Yojson.Safe.from_string raw in
            let from_json =
              {
                colo = json_string_field json "colo";
                city = json_string_field json "city";
                country = json_string_field json "country";
                asn = json_string_field json "asn";
                ip = json_string_field json "clientIp";
              }
            in
            merge_server_meta from_json (server_of_headers headers))
      with _ -> empty_server_meta)

let measure_latency_once ~clock ~client ~server_ref plan endpoints =
  try
    let started = Unix.gettimeofday () in
    Eio.Time.with_timeout_exn clock plan.request_timeout_s (fun () ->
        Eio.Switch.run @@ fun sw ->
        let response, body =
          Client.get client ~sw (download_uri endpoints 0)
        in
        let responded = Unix.gettimeofday () in
        let headers = Response.headers response in
        server_ref := merge_server_meta !server_ref (server_of_headers headers);
        let status = Response.status response in
        let status_code = Code.code_of_status status in
        ignore (drain_and_count body);
        if is_success status then
          Some
            {
              ping_ms = (responded -. started) *. 1000.;
              status_code;
              timestamp_s = started;
            }
        else
          None)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> None

let rec collect_idle_latency ~clock ~client ~server_ref plan endpoints ~total
    ~on_attempt remaining acc =
  if remaining <= 0 then
    List.rev acc
  else
    let sample = measure_latency_once ~clock ~client ~server_ref plan endpoints in
    let completed = total - remaining + 1 in
    on_attempt ~completed sample;
    let acc =
      match sample with
      | Some sample -> sample :: acc
      | None -> acc
    in
    if remaining = 1 then
      List.rev acc
    else (
      Eio.Time.sleep clock plan.latency_interval_s;
      collect_idle_latency ~clock ~client ~server_ref plan endpoints ~total
        ~on_attempt (remaining - 1) acc)

let transfer_one ~clock ~client ~direction ~server_ref plan endpoints
    request_bytes =
  try
    let started = Unix.gettimeofday () in
    Eio.Time.with_timeout_exn clock plan.request_timeout_s (fun () ->
        Eio.Switch.run @@ fun sw ->
        match direction with
        | Download ->
            let response, body =
              Client.get client ~sw (download_uri endpoints request_bytes)
            in
            let responded = Unix.gettimeofday () in
            let headers = Response.headers response in
            server_ref :=
              merge_server_meta !server_ref (server_of_headers headers);
            let status = Response.status response in
            let status_code = Code.code_of_status status in
            let transfer_bytes = drain_and_count body in
            let finished = Unix.gettimeofday () in
            let duration_s = finished -. started in
            if is_success status && transfer_bytes > 0 && duration_s > 0. then
              Some
                {
                  request_bytes;
                  transfer_bytes;
                  duration_s;
                  bps = (float_of_int transfer_bytes *. 8.) /. duration_s;
                  ttfb_ms = (responded -. started) *. 1000.;
                  status_code;
                }
            else
              None
        | Upload ->
            let payload = payload_of_size request_bytes in
            let headers =
              Header.init ()
              |> fun headers ->
              Header.add headers "content-type" "application/octet-stream"
              |> fun headers ->
              Header.add headers "content-length" (string_of_int request_bytes)
            in
            let response, body =
              Client.call client ~sw ~headers ~body:(Body.of_string payload)
                `POST (upload_uri endpoints)
            in
            let responded = Unix.gettimeofday () in
            let response_headers = Response.headers response in
            server_ref :=
              merge_server_meta !server_ref (server_of_headers response_headers);
            let status = Response.status response in
            let status_code = Code.code_of_status status in
            ignore (drain_and_count body);
            let finished = Unix.gettimeofday () in
            let duration_s = finished -. started in
            if is_success status && duration_s > 0. then
              Some
                {
                  request_bytes;
                  transfer_bytes = request_bytes;
                  duration_s;
                  bps = (float_of_int request_bytes *. 8.) /. duration_s;
                  ttfb_ms = (responded -. started) *. 1000.;
                  status_code;
                }
            else
              None)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> None

let race_stop stop fn =
  Eio.Fiber.first
    (fun () ->
      Eio.Promise.await stop;
      `Stop)
    (fun () -> `Value (fn ()))

let rec side_probe_loop ~clock ~client ~server_ref plan endpoints ~on_sample
    stop =
  match
    race_stop stop (fun () ->
        measure_latency_once ~clock ~client ~server_ref plan endpoints)
  with
  | `Stop -> ()
  | `Value sample ->
      Option.iter on_sample sample;
      (match
         race_stop stop (fun () -> Eio.Time.sleep clock plan.side_probe_interval_s)
       with
      | `Stop -> ()
      | `Value () ->
          side_probe_loop ~clock ~client ~server_ref plan endpoints
            ~on_sample stop)

let run_set ~clock ~client ~direction ~server_ref plan endpoints request_bytes
    count ~on_update =
  let stop, stopper = Eio.Promise.create () in
  let current_set = ref (empty_bandwidth_set request_bytes) in
  let completed = ref 0 in
  let emit_update () =
    on_update ~set:!current_set ~completed:!completed ~total:count
  in
  let on_side_sample sample =
    current_set := add_side_latency !current_set sample;
    emit_update ()
  in
  emit_update ();
  Eio.Switch.run @@ fun sw ->
  let probe =
    Eio.Fiber.fork_promise ~sw (fun () ->
        side_probe_loop ~clock ~client ~server_ref plan endpoints
          ~on_sample:on_side_sample stop)
  in
  let workers =
    List.init count (fun _ ->
        Eio.Fiber.fork_promise ~sw (fun () ->
            let sample =
              transfer_one ~clock ~client ~direction ~server_ref plan endpoints
                request_bytes
            in
            completed := !completed + 1;
            (match sample with
            | Some sample ->
                current_set := add_bandwidth_sample !current_set sample
            | None -> ());
            emit_update ()))
  in
  List.iter Eio.Promise.await_exn workers;
  ignore (Eio.Promise.try_resolve stopper ());
  Eio.Promise.await_exn probe;
  !current_set

let direction_parallel plan = function
  | Download -> plan.download_parallel
  | Upload -> plan.upload_parallel

let direction_sizes plan = function
  | Download -> plan.download_sizes
  | Upload -> plan.upload_sizes

let build_direction_result (plan : plan) (sets : bandwidth_set list) :
    direction_result =
  let samples =
    List.concat_map (fun (set : bandwidth_set) -> set.samples) sets
  in
  let qualified_samples =
    List.filter
      (fun sample -> sample.duration_s >= plan.min_sample_duration_s)
      samples
  in
  let bandwidth_bps =
    Stats.percentile
      (List.map (fun sample -> sample.bps) qualified_samples)
      plan.bandwidth_percentile
  in
  let loaded_sets =
    List.filter
      (fun (set : bandwidth_set) ->
        match set.min_duration_s with
        | Some duration -> duration >= plan.loaded_latency_min_s
        | None -> false)
      sets
  in
  let loaded_points =
    loaded_sets
    |> List.concat_map (fun (set : bandwidth_set) ->
           List.map (fun sample -> sample.ping_ms) set.side_latencies)
    |> Stats.take_last plan.loaded_max_points
  in
  {
    bandwidth_bps;
    samples;
    qualified_samples = List.length qualified_samples;
    sets;
    loaded_latency_ms =
      Stats.percentile loaded_points plan.latency_percentile;
    loaded_jitter_ms = Stats.jitter loaded_points;
    loaded_points;
    selected_request_bytes =
      (match List.rev sets with
      | [] -> None
      | last :: _ -> Some last.request_bytes);
  }

let direction_result_of_sets (plan : plan) sets =
  match sets with
  | [] -> None
  | _ -> Some (build_direction_result plan sets)

let progress_snapshot config started_at phase server idle_points download_sets
    upload_sets =
  {
    started_at;
    preset = config.preset;
    phase;
    server;
    idle_points;
    download_enabled = config.enable_download;
    upload_enabled = config.enable_upload;
    download = direction_result_of_sets config.plan download_sets;
    upload = direction_result_of_sets config.plan upload_sets;
  }

let rec run_direction_sets ~clock ~client ~direction ~server_ref plan endpoints
    sizes completed ~total_sets ~set_index ~on_update =
  match sizes with
  | [] -> completed
  | request_bytes :: rest ->
      let set =
        run_set ~clock ~client ~direction ~server_ref plan endpoints request_bytes
          (direction_parallel plan direction)
          ~on_update:(fun ~set ~completed:sample_count ~total ->
            on_update ~sets:(completed @ [ set ]) ~set_index ~request_bytes
              ~completed:sample_count ~total)
      in
      let completed = completed @ [ set ] in
      let saturated =
        match set.min_duration_s with
        | Some duration -> duration >= plan.saturation_target_s
        | None -> false
      in
      if saturated || rest = [] then
        completed
      else
        run_direction_sets ~clock ~client ~direction ~server_ref plan endpoints
          rest completed ~total_sets ~set_index:(set_index + 1) ~on_update

let measure_direction ~clock ~client ~direction ~server_ref plan endpoints
    ~on_update =
  let sizes = direction_sizes plan direction in
  let total_sets = List.length sizes in
  let sets =
    run_direction_sets ~clock ~client ~direction ~server_ref plan endpoints sizes []
      ~total_sets ~set_index:1 ~on_update
  in
  build_direction_result plan sets

let run ?on_progress config =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  let started_at = Unix.gettimeofday () in
  let phase_ref = ref Fetching_meta in
  let server_ref = ref empty_server_meta in
  let idle_points_ref = ref [] in
  let download_sets_ref = ref [] in
  let upload_sets_ref = ref [] in
  let client = make_client env in
  let emit () =
    match on_progress with
    | None -> ()
    | Some callback ->
        callback
          (progress_snapshot config started_at !phase_ref !server_ref
             !idle_points_ref !download_sets_ref !upload_sets_ref)
  in
  emit ();
  let initial_server =
    fetch_server_meta ~clock:env#clock ~client config.endpoints
  in
  server_ref := initial_server;
  phase_ref := Idle_latency { completed = 0; total = config.plan.latency_samples };
  emit ();
  let idle_samples =
    collect_idle_latency ~clock:env#clock ~client ~server_ref config.plan
      config.endpoints ~total:config.plan.latency_samples
      ~on_attempt:(fun ~completed sample ->
        (match sample with
        | Some sample -> idle_points_ref := !idle_points_ref @ [ sample.ping_ms ]
        | None -> ());
        phase_ref :=
          Idle_latency { completed; total = config.plan.latency_samples };
        emit ())
      config.plan.latency_samples []
  in
  let idle_points = List.map (fun sample -> sample.ping_ms) idle_samples in
  idle_points_ref := idle_points;
  let idle_latency_ms =
    Stats.percentile idle_points config.plan.latency_percentile
  in
  let idle_jitter_ms = Stats.jitter idle_points in
  let download =
    if config.enable_download then
      Some
        (measure_direction ~clock:env#clock ~client ~direction:Download
           ~server_ref config.plan config.endpoints
           ~on_update:(fun ~sets ~set_index ~request_bytes ~completed ~total ->
             download_sets_ref := sets;
             phase_ref :=
               Downloading
                 {
                   set_index;
                   total_sets = List.length config.plan.download_sizes;
                   request_bytes;
                   completed;
                   total;
                 };
             emit ()))
    else
      None
  in
  let upload =
    if config.enable_upload then
      Some
        (measure_direction ~clock:env#clock ~client ~direction:Upload
           ~server_ref config.plan config.endpoints
           ~on_update:(fun ~sets ~set_index ~request_bytes ~completed ~total ->
             upload_sets_ref := sets;
             phase_ref :=
               Uploading
                 {
                   set_index;
                   total_sets = List.length config.plan.upload_sizes;
                   request_bytes;
                   completed;
                   total;
                 };
             emit ()))
    else
      None
  in
  phase_ref := Finished;
  emit ();
  {
    generated_at = Unix.gettimeofday ();
    preset = config.preset;
    server = !server_ref;
    idle_latency_ms;
    idle_jitter_ms;
    idle_points;
    download;
    upload;
  }
