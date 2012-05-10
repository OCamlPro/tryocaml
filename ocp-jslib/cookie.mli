
(** Gets browser cookies and returns a [(key, value) list].  *)
val get_cookie : unit -> (string * string) list

(** Sets browser cookies. Expiration time is one year by default. *)
val set_cookie : string -> string -> unit

(** Sets browser cookies with expiration time. *)
val set_cookie_with_timeout : string -> string -> Js.date Js.t -> unit



val initial_cookies : (string * string) list

