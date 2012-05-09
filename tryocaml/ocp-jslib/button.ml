open Utils

let registered_buttons = ref []

let button_type = _s "button"

let create txt action =
  let b = Dom_html.createButton ~_type:button_type doc in
  let id = "button" ^ txt in
  b##innerHTML <- _s txt;
  b##id <- _s id;
  registered_buttons := (id, txt) :: !registered_buttons;
  b##className <- _s "btn";
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b

let create_with_image src width txt action =
  let b = Dom_html.createButton ~_type:button_type doc in
  let id = "button" ^ txt in
  b##innerHTML <- 
    Js.string (Printf.sprintf 
                 "<img src=\"%s\" width=\"%d\" text=\"%s\"/>" src width txt);
  b##id <- Js.string id;
  b##className <- _s "btn";
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b
