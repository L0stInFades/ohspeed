val run :
  ?on_progress:(Model.progress -> unit) -> Model.run_config -> Model.report Lwt.t
