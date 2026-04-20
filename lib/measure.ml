open Lwt.Syntax
open Lwt.Infix

module Body = Cohttp_lwt.Body
module Client = Cohttp_lwt_unix.Client
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

let fetch_server_meta endpoints =
  match endpoints.meta_url with
  | None -> Lwt.return empty_server_meta
  | Some url ->
      Lwt.catch
        (fun () ->
          let* response, body =
            Lwt_unix.with_timeout 5. (fun () -> Client.get (add_cache_bust url))
          in
          let headers = Response.headers response in
          let* raw = Body.to_string body in
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
          Lwt.return (merge_server_meta from_json (server_of_headers headers)))
        (fun _ -> Lwt.return empty_server_meta)

let drain_and_count body =
  let total = ref 0 in
  let stream = Body.to_stream body in
  let* () =
    Lwt_stream.iter_s
      (fun chunk ->
        total := !total + String.length chunk;
        Lwt.return_unit)
      stream
  in
  Lwt.return !total

let measure_latency_once ~server_ref plan endpoints =
  Lwt.catch
    (fun () ->
      let started = Unix.gettimeofday () in
      let* response, body =
        Lwt_unix.with_timeout plan.request_timeout_s (fun () ->
            Client.get (download_uri endpoints 0))
      in
      let responded = Unix.gettimeofday () in
      let headers = Response.headers response in
      server_ref := merge_server_meta !server_ref (server_of_headers headers);
      let status = Response.status response in
      let status_code = Code.code_of_status status in
      let* _ = Body.to_string body in
      if is_success status then
        Lwt.return_some
          { ping_ms = (responded -. started) *. 1000.; status_code; timestamp_s = started }
      else
        Lwt.return_none)
    (fun _ -> Lwt.return_none)

let rec collect_idle_latency ~server_ref plan endpoints ~total ~on_attempt
    remaining acc =
  if remaining <= 0 then
    Lwt.return (List.rev acc)
  else
    let* sample = measure_latency_once ~server_ref plan endpoints in
    let completed = total - remaining + 1 in
    on_attempt ~completed sample;
    let acc =
      match sample with
      | Some sample -> sample :: acc
      | None -> acc
    in
    if remaining = 1 then
      Lwt.return (List.rev acc)
    else
      let* () = Lwt_unix.sleep plan.latency_interval_s in
      collect_idle_latency ~server_ref plan endpoints ~total ~on_attempt
        (remaining - 1) acc

let transfer_one ~direction ~server_ref plan endpoints request_bytes =
  Lwt.catch
    (fun () ->
      let started = Unix.gettimeofday () in
      match direction with
      | Download ->
          let* response, body =
            Lwt_unix.with_timeout plan.request_timeout_s (fun () ->
                Client.get (download_uri endpoints request_bytes))
          in
          let responded = Unix.gettimeofday () in
          let headers = Response.headers response in
          server_ref := merge_server_meta !server_ref (server_of_headers headers);
          let status = Response.status response in
          let status_code = Code.code_of_status status in
          let* transfer_bytes = drain_and_count body in
          let finished = Unix.gettimeofday () in
          let duration_s = finished -. started in
          if is_success status && transfer_bytes > 0 && duration_s > 0. then
            Lwt.return_some
              {
                request_bytes;
                transfer_bytes;
                duration_s;
                bps = (float_of_int transfer_bytes *. 8.) /. duration_s;
                ttfb_ms = (responded -. started) *. 1000.;
                status_code;
              }
          else
            Lwt.return_none
      | Upload ->
          let payload = payload_of_size request_bytes in
          let headers =
            Header.init ()
            |> fun headers ->
            Header.add headers "content-type" "application/octet-stream"
            |> fun headers ->
            Header.add headers "content-length" (string_of_int request_bytes)
          in
          let* response, body =
            Lwt_unix.with_timeout plan.request_timeout_s (fun () ->
                Client.call `POST ~headers ~body:(Body.of_string payload)
                  (upload_uri endpoints))
          in
          let responded = Unix.gettimeofday () in
          let response_headers = Response.headers response in
          server_ref :=
            merge_server_meta !server_ref (server_of_headers response_headers);
          let status = Response.status response in
          let status_code = Code.code_of_status status in
          let* _ = drain_and_count body in
          let finished = Unix.gettimeofday () in
          let duration_s = finished -. started in
          if is_success status && duration_s > 0. then
            Lwt.return_some
              {
                request_bytes;
                transfer_bytes = request_bytes;
                duration_s;
                bps = (float_of_int request_bytes *. 8.) /. duration_s;
                ttfb_ms = (responded -. started) *. 1000.;
                status_code;
              }
          else
            Lwt.return_none)
    (fun _ -> Lwt.return_none)

let rec side_probe_loop ~server_ref plan endpoints ~on_sample stop =
  let* step =
    Lwt.pick
      [
        (stop >|= fun () -> `Stop);
        (measure_latency_once ~server_ref plan endpoints >|= fun sample ->
         `Sample sample);
      ]
  in
  match step with
  | `Stop -> Lwt.return_unit
  | `Sample sample ->
      Option.iter on_sample sample;
      let* gate =
        Lwt.pick
          [
            (stop >|= fun () -> `Stop);
            (Lwt_unix.sleep plan.side_probe_interval_s >|= fun () -> `Continue);
          ]
      in
      (match gate with
      | `Stop -> Lwt.return_unit
      | `Continue ->
          side_probe_loop ~server_ref plan endpoints ~on_sample stop)

let run_set ~direction ~server_ref plan endpoints request_bytes count ~on_update
    =
  let stop, stopper = Lwt.wait () in
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
  let probe =
    side_probe_loop ~server_ref plan endpoints ~on_sample:on_side_sample stop
  in
  let workers = List.init count Fun.id in
  let* () =
    Lwt.finalize
      (fun () ->
        Lwt_list.iter_p
            (fun _ ->
              let* sample =
                transfer_one ~direction ~server_ref plan endpoints request_bytes
              in
              completed := !completed + 1;
              (match sample with
              | Some sample -> current_set := add_bandwidth_sample !current_set sample
              | None -> ());
              emit_update ();
              Lwt.return_unit)
            workers
      )
      (fun () ->
        if Lwt.is_sleeping stop then Lwt.wakeup_later stopper ();
        Lwt.return_unit)
  in
  let* () = probe in
  Lwt.return !current_set

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

let rec run_direction_sets ~direction ~server_ref plan endpoints sizes completed
    ~total_sets ~set_index ~on_update =
  match sizes with
  | [] -> Lwt.return completed
  | request_bytes :: rest ->
      let* set =
        run_set ~direction ~server_ref plan endpoints request_bytes
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
        Lwt.return completed
      else
        run_direction_sets ~direction ~server_ref plan endpoints rest completed
          ~total_sets ~set_index:(set_index + 1) ~on_update

let measure_direction ~direction ~server_ref plan endpoints ~on_update =
  let sizes = direction_sizes plan direction in
  let total_sets = List.length sizes in
  let* sets =
    run_direction_sets ~direction ~server_ref plan endpoints sizes []
      ~total_sets ~set_index:1 ~on_update
  in
  Lwt.return (build_direction_result plan sets)

let run ?on_progress config =
  Random.self_init ();
  let started_at = Unix.gettimeofday () in
  let phase_ref = ref Fetching_meta in
  let server_ref = ref empty_server_meta in
  let idle_points_ref = ref [] in
  let download_sets_ref = ref [] in
  let upload_sets_ref = ref [] in
  let emit () =
    match on_progress with
    | None -> ()
    | Some callback ->
        callback
          (progress_snapshot config started_at !phase_ref !server_ref
             !idle_points_ref !download_sets_ref !upload_sets_ref)
  in
  emit ();
  let* initial_server = fetch_server_meta config.endpoints in
  server_ref := initial_server;
  phase_ref := Idle_latency { completed = 0; total = config.plan.latency_samples };
  emit ();
  let* idle_samples =
    collect_idle_latency ~server_ref config.plan config.endpoints
      ~total:config.plan.latency_samples
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
  let* download =
    if config.enable_download then
      let* result =
        measure_direction ~direction:Download ~server_ref config.plan
          config.endpoints
          ~on_update:(fun ~sets ~set_index ~request_bytes ~completed ~total ->
            download_sets_ref := sets;
            phase_ref :=
              Downloading
                { set_index; total_sets = List.length config.plan.download_sizes; request_bytes; completed; total };
            emit ())
      in
      Lwt.return_some result
    else
      Lwt.return_none
  in
  let* upload =
    if config.enable_upload then
      let* result =
        measure_direction ~direction:Upload ~server_ref config.plan
          config.endpoints
          ~on_update:(fun ~sets ~set_index ~request_bytes ~completed ~total ->
            upload_sets_ref := sets;
            phase_ref :=
              Uploading
                { set_index; total_sets = List.length config.plan.upload_sizes; request_bytes; completed; total };
            emit ())
      in
      Lwt.return_some result
    else
      Lwt.return_none
  in
  phase_ref := Finished;
  emit ();
  Lwt.return
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
