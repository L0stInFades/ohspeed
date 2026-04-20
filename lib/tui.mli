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
val render_live : ansi:bool -> history:History.entry list -> Model.progress -> string
val render_history : ansi:bool -> history:History.entry list -> string
val render_home :
  ansi:bool ->
  history:History.entry list ->
  settings:interactive_settings ->
  selected:int ->
  string
val render_settings :
  ansi:bool ->
  history:History.entry list ->
  settings:interactive_settings ->
  selected:int ->
  string
val render_history_browser :
  ansi:bool ->
  history:History.entry list ->
  settings:interactive_settings ->
  string
val render_result :
  ansi:bool ->
  history:History.entry list ->
  settings:interactive_settings ->
  report:Model.report ->
  string
val render_notice :
  ansi:bool ->
  title:string ->
  subtitle:string ->
  detail_lines:string list ->
  footer_lines:string list ->
  string
