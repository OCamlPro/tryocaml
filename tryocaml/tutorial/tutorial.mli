
val this_lesson : int ref
val this_lesson_title : string ref
val this_lesson_html : string ref
val this_step : int ref
val this_step_title : string ref
val this_step_html : string ref

val lesson: int -> unit
val step : int -> unit
val next : unit -> unit
val back : unit -> unit
val check_step : Format.formatter -> string -> string -> unit

val debug : bool -> unit

val lessons_table : (string * string * (string * (string * string)) list * (string * string * (string * (string * string)) list * (string -> string -> bool)) option array) option array

val debug_fun : (string -> unit) ref
val message_fun : (string -> unit) ref
val clear_fun : (unit -> unit) ref

val clear : unit -> unit

val lessons : unit -> unit
val steps : unit -> unit
val print_debug : string -> unit

val use_multiline : bool ref
val multiline : bool -> unit

val set_lang : string -> unit
val lang : unit -> string
