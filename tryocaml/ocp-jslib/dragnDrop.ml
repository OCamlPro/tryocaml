
open Utils

type drag_events = {
  mutable ondragstart : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondragover  : Dom_html.dragEvent Js.t -> bool Js.t;
  mutable ondragend   : Dom_html.dragEvent Js.t  -> bool Js.t;
  mutable ondrop      : Dom_html.dragEvent Js.t ->  bool Js.t;
  mutable ondragleave : Dom_html.dragEvent Js.t ->  bool Js.t;
  mutable ondrag      : Dom_html.dragEvent Js.t ->  bool Js.t;
}

let default_func = (fun e -> Js._false)

let default_event = {
  ondragover      = default_func;
  ondragend       = default_func;
  ondrop          = default_func;
  ondragstart     = default_func;
  ondrag          = default_func;
  ondragleave     = default_func;
}
         
let init () = {
  ondragover      = default_func;
  ondragend       = default_func;
  ondrop          = default_func;
  ondragstart     = default_func;
  ondrag          = default_func;
  ondragleave     = default_func;
}    

let make ?events:(ev=default_event) container =
  container##ondragover <- Dom.handler ev.ondragover;
  container##ondragend <- Dom.handler ev.ondragend;
  container##ondrop  <- Dom_html.handler ev.ondrop








