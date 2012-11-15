
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
val update_lang_fun : (unit -> unit) ref
val read_fun : (string -> string -> string) ref
val read_bool : unit -> bool
val read_string : unit -> string
val read_int : unit -> int
val read_float : unit -> float


val lessons : unit -> unit
val steps : unit -> unit
val print_debug : string -> unit

val use_multiline : bool ref
val multiline : bool -> unit

val set_lang : string -> unit
val lang : unit -> string

val langs : (string * (string * (string,string) Hashtbl.t) ) list

val translate : string -> string

external int_of_int : int -> int = "%identity"
external nativeint_of_nativeint : nativeint -> nativeint = "%identity"
external float_of_float : float -> float = "%identity"
external int32_of_int32 : int32 -> int32 = "%identity"
external int64_of_int64 : int64 -> int64 = "%identity"

external int_to_int : int -> int = "%identity"
external float_to_float : float -> float = "%identity"

module Tutorial : sig end

val init : unit -> unit
