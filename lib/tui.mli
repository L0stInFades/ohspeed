type session
type interactive_settings = {
  enable_download : bool;
  enable_upload : bool;
  save_history : bool;
  history_limit : int;
}

val sparkline : width:int -> float list -> string
val create_session : out_channel -> session
val start : session -> unit
val render : session -> string -> unit
val stop : session -> unit
val render_transition :
  ansi:bool ->
  animation_tick:int ->
  accent:int ->
  label:string ->
  progress:float ->
  string
val render_live :
  ansi:bool ->
  animation_tick:int ->
  history:History.entry list ->
  Model.progress ->
  string
val render_history :
  ansi:bool -> animation_tick:int -> history:History.entry list -> string
val render_home :
  ansi:bool ->
  animation_tick:int ->
  history:History.entry list ->
  settings:interactive_settings ->
  selected:int ->
  string
val render_settings :
  ansi:bool ->
  animation_tick:int ->
  history:History.entry list ->
  settings:interactive_settings ->
  selected:int ->
  string
val render_history_browser :
  ansi:bool ->
  animation_tick:int ->
  history:History.entry list ->
  settings:interactive_settings ->
  scale_label:string ->
  string
val render_result :
  ansi:bool ->
  animation_tick:int ->
  history:History.entry list ->
  settings:interactive_settings ->
  report:Model.report ->
  string
val render_notice :
  ansi:bool ->
  animation_tick:int ->
  title:string ->
  subtitle:string ->
  detail_lines:string list ->
  footer_lines:string list ->
  string
