open Lwt.Infix
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

type state = {
  mutable screen : screen;
  mutable home_index : int;
  mutable settings_index : int;
  mutable settings : Tui.interactive_settings;
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
  History.load ~limit:state.settings.history_limit ()

let render_screen ~ansi state =
  let history = current_history state in
  match state.screen with
  | Home ->
      Tui.render_home ~ansi ~history ~settings:state.settings
        ~selected:state.home_index
  | Settings ->
      Tui.render_settings ~ansi ~history ~settings:state.settings
        ~selected:state.settings_index
  | History ->
      Tui.render_history_browser ~ansi ~history ~settings:state.settings
  | Result report ->
      Tui.render_result ~ansi ~history ~settings:state.settings ~report
  | Alert message ->
      Tui.render_notice ~ansi ~title:" ohspeed Alert "
        ~subtitle:"Something needs attention"
        ~detail_lines:
          [
            message;
            Tui.sparkline ~width:28 [];
            "Press Enter, b, or Esc to return home.";
          ]
        ~footer_lines:
          [
            "q quits the app immediately.";
            Printf.sprintf "History window: %d runs" state.settings.history_limit;
          ]

let run_measurement ~ansi ~session ~endpoints state preset =
  if not state.settings.enable_download && not state.settings.enable_upload then
    state.screen <- Alert "Download and upload cannot both be disabled."
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
    let result =
      Lwt_main.run
        (Lwt.catch
           (fun () ->
             Measure.run
               ~on_progress:(fun progress ->
                 Tui.render session (Tui.render_live ~ansi ~history progress))
               config
             >|= fun report -> Stdlib.Ok report)
           (fun exn -> Lwt.return (Stdlib.Error (Printexc.to_string exn))))
    in
    match result with
    | Stdlib.Error message -> state.screen <- Alert message
    | Stdlib.Ok report ->
        if state.settings.save_history then
          ignore (History.append report);
        state.screen <- Result report

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
  | _ -> state.screen <- Home

let handle_home_key ~ansi ~session ~endpoints state = function
  | Up | Char 'k' ->
      state.home_index <- wrap_index home_item_count state.home_index (-1)
  | Down | Char 'j' ->
      state.home_index <- wrap_index home_item_count state.home_index 1
  | Char '1' -> run_measurement ~ansi ~session ~endpoints state Quick
  | Char '2' -> run_measurement ~ansi ~session ~endpoints state Balanced
  | Char '3' -> run_measurement ~ansi ~session ~endpoints state Full
  | Char 'h' | Char 'H' -> state.screen <- History
  | Char 's' | Char 'S' -> state.screen <- Settings
  | Char 'q' | Char 'Q' | Escape -> raise Quit
  | Enter -> (
      match state.home_index with
      | 0 -> run_measurement ~ansi ~session ~endpoints state Quick
      | 1 -> run_measurement ~ansi ~session ~endpoints state Balanced
      | 2 -> run_measurement ~ansi ~session ~endpoints state Full
      | 3 -> state.screen <- History
      | 4 -> state.screen <- Settings
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
  | Char 'b' | Char 'B' | Escape -> state.screen <- Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let handle_history_key state = function
  | Char 'b' | Char 'B' | Escape | Enter -> state.screen <- Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let handle_result_key ~ansi ~session ~endpoints state
    (report : Model.report) = function
  | Enter | Char 'r' | Char 'R' ->
      run_measurement ~ansi ~session ~endpoints state report.preset
  | Char 'h' | Char 'H' -> state.screen <- History
  | Char 'b' | Char 'B' | Escape -> state.screen <- Home
  | Char 'q' | Char 'Q' -> raise Quit
  | _ -> ()

let handle_error_key state = function
  | Enter | Char 'b' | Char 'B' | Escape -> state.screen <- Home
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
                Tui.render session (render_screen ~ansi state);
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
