type session

val sparkline : width:int -> float list -> string
val create_session : out_channel -> session
val start : session -> unit
val render : session -> string -> unit
val stop : session -> unit
val render_live : ansi:bool -> history:History.entry list -> Model.progress -> string
val render_history : ansi:bool -> history:History.entry list -> string
