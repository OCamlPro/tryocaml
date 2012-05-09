let document = Dom_html.document
let window = Dom_html.window

let _s s = Js.string s
let _f f = Js.wrap_callback f

let getElementById id =
  Js.Opt.get (document##getElementById (_s id)) (fun () -> assert false)

let createTextButton id txt action =
  let b = Dom_html.createButton ~_type:(_s "button") document in
  b##innerHTML <- _s txt;
  b##id <- _s id;
  b##className <- _s "btn";
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b

let setIntervalUntilFalse f time =
  let interval_id = ref None in
  let f () =
    if not (f ()) then
      match !interval_id with
          None -> ()
        | Some interval_id ->
          window##clearInterval (interval_id)
  in
  interval_id := Some (window##setInterval (_f f, time))

let setInterval f time =
  let interval_id = window##setInterval (_f f, time) in
  (fun _ -> window##clearInterval (interval_id))

let setTimeout f time =
  let interval_id = window##setTimeout (_f f, time) in
  (fun _ -> window##clearTimeout (interval_id))


(***************************************)

let canvas = Dom_html.createCanvas document
let body = getElementById "main-title"
let context = canvas##getContext (Dom_html._2d_)

let x = ref 10.
let y = ref 60.

let redraw _ =
  context##clearRect (!x, !y, 20., 20. );
  x := if !x > 400. then 0. else !x +. 2.;
  y := if !y > 150. then 0. else !y +. 1.;
  context##fillStyle <- Js.string "red";
  context##fillRect (!x,!y, 20., 20.)

let _ =
  Dom.appendChild body canvas;
  let stop = setInterval redraw 2.5 in
  Dom.appendChild body (createTextButton "stop" "Stop" stop);
  setTimeout stop 50000.




