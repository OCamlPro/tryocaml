let document = Dom_html.document
let window = Dom_html.window

let _s s = Js.string s
let _f f = Js.wrap_callback f

let getElementById id =
  Js.Opt.get (document##getElementById (_s id)) (fun () -> assert false)

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
  let interval_id = window##setInterval (_f redraw, 2.5) in
  ()





