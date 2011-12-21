
val this_lesson : int ref
val this_step : int ref
val this_step_txt : string ref

val lesson: int -> unit
val step : int -> unit
val check_step : Format.formatter -> string -> string -> unit


val debug : bool -> unit

val lessons :
(string * (string -> string -> bool)) option array option array
