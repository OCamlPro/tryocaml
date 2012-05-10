open Utils

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

let canvas = Dom_html.createCanvas doc
let body = get_element_by_id "main-title"
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
  Dom.appendChild body (Button.create "Stop" stop);
  setTimeout stop 50000.




