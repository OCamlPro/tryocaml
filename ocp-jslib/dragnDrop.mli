
(** By default, all events return false. *)
type drag_events = {
  mutable ondragstart : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondragover  : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondragend   : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondrop      : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondragleave : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondrag      : Dom_html.dragEvent Js.t -> bool Js.t;
}

(** Initializes a [drag_events] type with default behavior for all events. *)
val init : unit -> drag_events

(** Makes an element 'drag and drop'able. *)
val make : ?events:drag_events -> Dom_html.element Js.t -> unit

