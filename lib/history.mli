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

val file_path : unit -> string
val load : ?limit:int -> unit -> entry list
val of_report : Model.report -> entry
val entry_to_yojson : entry -> Yojson.Safe.t
val entry_of_yojson : Yojson.Safe.t -> entry option
val append : ?retain:int -> Model.report -> (unit, string) result
