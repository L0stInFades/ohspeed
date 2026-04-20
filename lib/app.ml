open Model

type key =
  | Up
  | Down
  | Left
  | Right
  | Enter
  | Escape
  | Char of char

type screen =
  | Home
  | Settings
  | History
  | Result of Model.report
  | Alert of string

type history_scale =
  | Day
  | Week
  | Month
  | All

type transition = {
  label : string;
  accent : int;
  started_at : float;
  duration_s : float;
}

type state = {
  mutable screen : screen;
  mutable home_index : int;
  mutable settings_index : int;
  mutable settings : Tui.interactive_settings;
  mutable history_scale : history_scale;
  mutable animation_tick : int;
  mutable transition : transition option;
}

exception Quit

let home_item_count = 6
let settings_item_count = 5

let default_settings =
  {
    Tui.enable_download = true;
    enable_upload = true;
    save_history = true;
    history_limit = 12;
  }

let wrap_index count current delta =
  (current + delta + count) mod count

let history_limit_options = [| 6; 12; 20; 40 |]
let history_scale_options = [| Day; Week; Month; All |]

let frame_interval_s = 1. /. 12.
let transition_duration_s = 0.18

let cycle_history_limit current delta =
  let current_index =
    let rec find index =
      if index >= Array.length history_limit_options then
        0
      else if history_limit_options.(index) = current then
        index
      else
        find (index + 1)
    in
    find 0
  in
  history_limit_options.(wrap_index (Array.length history_limit_options) current_index delta)

let cycle_history_scale current delta =
  let current_index =
    let rec find index =
      if index >= Array.length history_scale_options then
        0
      else if history_scale_options.(index) = current then
        index
      else
        find (index + 1)
    in
    find 0
  in
  history_scale_options.(wrap_index (Array.length history_scale_options) current_index delta)

let history_scale_label = function
  | Day -> "24h"
  | Week -> "7d"
  | Month -> "30d"
  | All -> "all"

let filter_history scale entries =
  let now = Unix.gettimeofday () in
  let horizon_s =
    match scale with
    | Day -> Some 86_400.
    | Week -> Some (7. *. 86_400.)
    | Month -> Some (30. *. 86_400.)
    | All -> None
  in
  match horizon_s with
  | None -> entries
  | Some horizon ->
      List.filter
        (fun (entry : History.entry) -> now -. entry.recorded_at <= horizon)
        entries

let screen_transition = function
  | Home -> (45, "launch pad")
  | Settings -> (111, "control deck")
  | History -> (214, "history lounge")
  | Result _ -> (118, "result deck")
  | Alert _ -> (196, "alert")

let set_screen state screen =
  let accent, label = screen_transition screen in
  state.screen <- screen;
  state.transition <-
    Some
      {
        label;
        accent;
        started_at = Unix.gettimeofday ();
        duration_s = transition_duration_s;
      }

let pause seconds =
  ignore (Unix.select [] [] [] seconds)

let stdin_fd () = Unix.descr_of_in_channel stdin
let stdout_fd () = Unix.descr_of_out_channel stdout

let wait_for_input fd timeout =
  match Unix.select [ fd ] [] [] timeout with
  | [], _, _ -> false
  | _ -> true

let read_byte fd =
  let buffer = Bytes.create 1 in
  match Unix.read fd buffer 0 1 with
  | 0 -> None
  | _ -> Some (Bytes.get buffer 0)

let read_key fd =
  match read_byte fd with
  | None -> Escape
  | Some ('\r' | '\n') -> Enter
  | Some '\027' ->
      if wait_for_input fd 0.01 then
        match read_byte fd with
        | Some '[' ->
            if wait_for_input fd 0.01 then
              (match read_byte fd with
              | Some 'A' -> Up
              | Some 'B' -> Down
              | Some 'C' -> Right
              | Some 'D' -> Left
              | Some ch -> Char ch
              | None -> Escape)
            else
              Escape
        | Some ch -> Char ch
        | None -> Escape
      else
        Escape
  | Some ch -> Char ch

let with_raw_input fd f =
  let saved = Unix.tcgetattr fd in
  let raw = Unix.tcgetattr fd in
  raw.Unix.c_icanon <- false;
  raw.Unix.c_echo <- false;
  raw.Unix.c_vmin <- 1;
  raw.Unix.c_vtime <- 0;
  Unix.tcsetattr fd Unix.TCSAFLUSH raw;
  Fun.protect
    ~finally:(fun () -> Unix.tcsetattr fd Unix.TCSAFLUSH saved)
    f

let current_history state =
  match state.screen with
  | History -> History.load () |> filter_history state.history_scale
  | _ -> History.load ~limit:state.settings.history_limit ()

let rec render_screen ~ansi state =
  let history = current_history state in
  match state.transition with
  | Some transition ->
      let elapsed = Unix.gettimeofday () -. transition.started_at in
      if elapsed < transition.duration_s then
        Tui.render_transition ~ansi ~animation_tick:state.animation_tick
          ~accent:transition.accent ~label:transition.label
          ~progress:(elapsed /. transition.duration_s)
      else (
        state.transition <- None;
        render_screen ~ansi state)
  | None -> (
      match state.screen with
      | Home ->
          Tui.render_home ~ansi ~animation_tick:state.animation_tick ~history
            ~settings:state.settings ~selected:state.home_index
      | Settings ->
          Tui.render_settings ~ansi ~animation_tick:state.animation_tick
            ~history ~settings:state.settings ~selected:state.settings_index
      | History ->
          Tui.render_history_browser ~ansi ~animation_tick:state.animation_tick
            ~history ~settings:state.settings
            ~scale_label:(history_scale_label state.history_scale)
      | Result report ->
          Tui.render_result ~ansi ~animation_tick:state.animation_tick ~history
            ~settings:state.settings ~report
      | Alert message ->
          Tui.render_notice ~ansi ~animation_tick:state.animation_tick
            ~title:" ohspeed Alert " ~subtitle:"Something needs attention"
            ~detail_lines:
              [
                message;
                Tui.sparkline ~width:28 [];
                "Press Enter, b, or Esc to return home.";
              ]
            ~footer_lines:
              [
                "q quits the app immediately.";
                Printf.sprintf "History window: %d runs"
                  state.settings.history_limit;
              ])

let run_measurement ~ansi ~session ~endpoints state preset =
  if not state.settings.enable_download && not state.settings.enable_upload then
    set_screen state (Alert "Download and upload cannot both be disabled.")
  else
    let config =
      {
        preset;
        plan = Model.plan_of_preset preset;
        endpoints;
        enable_download = state.settings.enable_download;
        enable_upload = state.settings.enable_upload;
      }
    in
    let history = current_history state in
    let measurement_tick = ref state.animation_tick in
    let accent =
      match preset with
      | Quick -> 45
      | Balanced -> 118
      | Full -> 201
    in
    Tui.render session
      (Tui.render_transition ~ansi ~animation_tick:!measurement_tick ~accent
         ~label:"arming measurement" ~progress:0.5);
    pause 0.08;
    let result =
      try
        Stdlib.Ok
          (Measure.run
              ~on_progress:(fun progress ->
                incr measurement_tick;
                Tui.render session
                  (Tui.render_live ~ansi ~animation_tick:!measurement_tick
                     ~history progress))
              config)
      with exn -> Stdlib.Error (Printexc.to_string exn)
    in
    match result with
    | Stdlib.Error message -> set_screen state (Alert message)
    | Stdlib.Ok report ->
        if state.settings.save_history then
          ignore (History.append report);
        set_screen state (Result report)

let apply_settings_change state direction =
  match state.settings_index with
  | 0 ->
      state.settings <-
        {
          state.settings with
          enable_download = not state.settings.enable_download;
        }
  | 1 ->
      state.settings <-
        {
          state.settings with
          enable_upload = not state.settings.enable_upload;
        }
  | 2 ->
      state.settings <-
        {
          state.settings with
          save_history = not state.settings.save_history;
        }
  | 3 ->
      state.settings <-
        {
          state.settings with
          history_limit =
            cycle_history_limit state.settings.history_limit direction;
        }
  | _ -> set_screen state Home

let handle_home_key ~ansi ~session ~endpoints state = function
  | Up | Char 'k' ->
      state.home_index <- wrap_index home_item_count state.home_index (-1)
  | Down | Char 'j' ->
      state.home_index <- wrap_index home_item_count state.home_index 1
  | Char '1' -> run_measurement ~ansi ~session ~endpoints state Quick
  | Char '2' -> run_measurement ~ansi ~session ~endpoints state Balanced
  | Char '3' -> run_measurement ~ansi ~session ~endpoints state Full
  | Char 'h' | Char 'H' -> set_screen state History
  | Char 's' | Char 'S' -> set_screen state Settings
  | Char 'q' | Char 'Q' | Escape -> raise Quit
  | Enter -> (
      match state.home_index with
      | 0 -> run_measurement ~ansi ~session ~endpoints state Quick
      | 1 -> run_measurement ~ansi ~session ~endpoints state Balanced
      | 2 -> run_measurement ~ansi ~session ~endpoints state Full
      | 3 -> set_screen state History
      | 4 -> set_screen state Settings
      | _ -> raise Quit)
  | _ -> ()

let handle_settings_key state = function
  | Up | Char 'k' ->
      state.settings_index <- wrap_index settings_item_count state.settings_index (-1)
  | Down | Char 'j' ->
      state.settings_index <- wrap_index settings_item_count state.settings_index 1
  | Left -> apply_settings_change state (-1)
  | Right -> apply_settings_change state 1
  | Enter -> apply_settings_change state 1
  | Char 'b' | Char 'B' | Escape -> set_screen state Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let handle_history_key state = function
  | Left | Char '[' -> state.history_scale <- cycle_history_scale state.history_scale (-1)
  | Right | Char ']' -> state.history_scale <- cycle_history_scale state.history_scale 1
  | Char '1' -> state.history_scale <- Day
  | Char '2' -> state.history_scale <- Week
  | Char '3' -> state.history_scale <- Month
  | Char '4' -> state.history_scale <- All
  | Char 'b' | Char 'B' | Escape | Enter -> set_screen state Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let handle_result_key ~ansi ~session ~endpoints state
    (report : Model.report) = function
  | Enter | Char 'r' | Char 'R' ->
      run_measurement ~ansi ~session ~endpoints state report.preset
  | Char 'h' | Char 'H' -> set_screen state History
  | Char 'b' | Char 'B' | Escape -> set_screen state Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let handle_error_key state = function
  | Enter | Char 'b' | Char 'B' | Escape -> set_screen state Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let run_interactive ~endpoints () =
  let input = stdin_fd () in
  let output = stdout_fd () in
  if not (Unix.isatty input && Unix.isatty output) then
    Error "interactive TUI requires a TTY on stdin and stdout"
  else
    let session = Tui.create_session stdout in
    let state =
        {
          screen = Home;
          home_index = 0;
          settings_index = 0;
          settings = default_settings;
          history_scale = Week;
          animation_tick = 0;
          transition = None;
        }
    in
    let ansi = true in
    with_raw_input input (fun () ->
        Fun.protect
          ~finally:(fun () -> Tui.stop session)
          (fun () ->
            Tui.start session;
            try
              while true do
                state.animation_tick <- state.animation_tick + 1;
                Tui.render session (render_screen ~ansi state);
                if wait_for_input input frame_interval_s then
                  let key = read_key input in
                  match state.screen with
                  | Home -> handle_home_key ~ansi ~session ~endpoints state key
                  | Settings -> handle_settings_key state key
                  | History -> handle_history_key state key
                  | Result report ->
                      handle_result_key ~ansi ~session ~endpoints state report key
                  | Alert _ -> handle_error_key state key
              done;
              Ok ()
            with Quit -> Ok ()))
