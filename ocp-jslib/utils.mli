(** {1 Useful functions and alias} *)
val doc : Dom_html.document Js.t

val window : Dom_html.window Js.t

val loc : Dom_html.location Js.t

val _s : string -> Js.js_string Js.t

val _f : ('a -> 'b) -> ('c, 'a -> 'b) Js.meth_callback

val set_div_by_id : string -> string -> unit

val set_by_id : string -> string -> unit

val get_element_by_id : string -> Dom_html.element Js.t

val get_by_id : string -> string

val get_by_name : string -> string

(** {2 Constructors}  *)

(** [jsnew0] is a function to build an object using contructor [constr]
    without arguments. *)
val jsnew0 : 'a Js.t Js.constr -> unit -> 'a Js.t

(** [jsnew1] is a function to build an object using contructor [constr]
    and argument [a].*)
val jsnew1 : ('a -> 'b Js.t) Js.constr -> 'a -> 'b Js.t

(** [jsnew2] is a function to build an object using contructor [constr]
    and arguments [a] and [b].*)
val jsnew2 : ('a -> 'b -> 'c Js.t) Js.constr -> 'a * 'b -> 'c Js.t

(** [jsnew3] is a function to build an object using contructor [constr]
    and arguments [a], [b] and [c].*)
val jsnew3 : ('a -> 'b -> 'c -> 'd Js.t) Js.constr -> 'a * 'b * 'c -> 'd Js.t

val setIntervalUntilFalse : (unit -> bool) -> float -> unit
val setInterval : (unit -> unit) -> float -> (unit -> unit)
val setTimeout : (unit -> unit) -> float -> (unit -> unit)
