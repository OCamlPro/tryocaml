val create : string -> (unit -> 'a) -> Dom_html.buttonElement Js.t

val create_with_image : string -> int -> string -> (unit -> 'a) -> Dom_html.buttonElement Js.t

val registered_buttons : (string * string) list ref
