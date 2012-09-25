
exception Graphic_failure of string

open Utils
(*
val open_graph : (string -> unit)
*)

type state = {
  context : Dom_html.canvasRenderingContext2D Js.t;
  mutable x : int;
  mutable y : int;
  mutable width : int;
  mutable height : int;
  mutable color : int;
  mutable line_width : int;
  mutable text_size : int;
  mutable font : string;
}

let state = ref None

let get_state () =
  match !state with
    None -> raise (Graphic_failure "Not initialized")
  | Some s -> s

let raw_set_color s =
(* TODO : do better ! *)
  s.context##fillStyle <- Js.string
    (Printf.sprintf "#%02x%02x%02x"
       ( (s.color lsr 16) land 0xff )
       ( (s.color lsr 8) land 0xff )
       ( (s.color lsr 0) land 0xff )
    )


(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* Initializations *)

let _ =
  Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

let set_window_title title =
  (* TODO *) ()
let resize_window width height =
  (* TODO *) ()
let clear_graph () =
  (* TODO *) ()
let size_x () =
  let s = get_state () in
  s.width

let size_y () =
  let s = get_state () in
  s.height

(* Double-buffering *)

let display_mode bool =
  (* TODO *) ()
let remember_mode bool =
  (* TODO *) ()
let synchronize unit =
  (* TODO *) ()

let auto_synchronize = function
  | true -> display_mode true; remember_mode true; synchronize ()
  | false -> display_mode false; remember_mode true
;;


(* Colors *)

type color = int

let rgb r g b = (r lsl 16) + (g lsl 8) + b

let set_color color =
  let s = get_state () in
  s.color <- color

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF

let background = white
and foreground = black

(* Drawing *)

let plot x y =
  let s = get_state () in
  s.x <- x;
  s.y <- y;
  raw_set_color s;
  let context = s.context in
  context##moveTo (float x, float (s.height - y));
  context##lineTo (float x, float (s.height - y))

let plots points =
  for i = 0 to Array.length points - 1 do
    let (x, y) = points.(i) in
    plot x y;
  done
;;

let point_color x y =
  failwith "Graphics.point_color not implemented"

let moveto x y =
  let s = get_state () in
  s.x <- x;
  s.y <- y

let current_x () =
  let s = get_state () in
  s.x
let current_y () =
  let s = get_state () in
  s.y

let current_point () = current_x (), current_y ()
let lineto x y =
  let s = get_state () in
  raw_set_color s;
  let context = s.context in
  context##moveTo (float s.x, float (s.height - s.y));
  context##lineTo (float x, float (s.height - y));
  s.x <- x;
  s.y <- y;
  ()


let rlineto x y = lineto (current_x () + x) (current_y () + y)
let rmoveto x y = moveto (current_x () + x) (current_y () + y)

let raw_draw_rect x y dx dy =
  let s = get_state () in
  raw_set_color s;
  s.context##strokeRect (float x, float (s.height - y), float dx, float dy)

let draw_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "draw_rect")
  else raw_draw_rect x y w h
;;

let draw_poly, draw_poly_line =
  let dodraw close_flag points =
    if Array.length points > 0 then begin
      let (savex, savey) = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      if close_flag then lineto (fst points.(0)) (snd points.(0));
      moveto savex savey;
    end;
  in dodraw true, dodraw false
;;
let draw_segments segs =
  let (savex, savey) = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey;
;;
let raw_draw_arc x y rx ry a1 a2 =
  failwith "Graphics.raw_draw_arc not implemented"

let draw_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "draw_arc/ellipse/circle")
  else raw_draw_arc x y rx ry a1 a2
;;

let draw_ellipse x y rx ry = draw_arc x y rx ry 0 360
let draw_circle x y r = draw_arc x y r r 0 360

let raw_set_line_width w =
  let s = get_state () in
  s.line_width <- w

let set_line_width w =
  if w < 0 then raise (Invalid_argument "set_line_width")
  else raw_set_line_width w
;;

let raw_fill_rect x y dx dy =
  let s = get_state () in
  raw_set_color s;
  s.context##fillRect (float x, float (s.height - y), float dx, float dy)

let fill_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "fill_rect")
  else raw_fill_rect x y w h
;;

let fill_poly point_array =
  failwith "Graphics.fill_poly not implemented"
let raw_fill_arc x y rx ry a1 a2 =
  failwith "Graphics.raw_fill_arc not implemented"

let fill_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "fill_arc/ellipse/circle")
  else raw_fill_arc x y rx ry a1 a2
;;

let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360
let fill_circle x y r = fill_arc x y r r 0 360

(* Text *)


let draw_string cs =
  let s = get_state () in
  let sdx = s.text_size * String.length cs in
  s.context##strokeText_withWidth
    (Js.string cs, float s.x, float (s.height - s.y), float sdx);
  s.x <- s.x + sdx

(*
  let m = s.context##measureText (Js.string cs) in
  let dx = m##width in (* TODO check !!! *)
*)
let draw_char c =
  let cs = String.make 1 c in draw_string cs

let set_font f =
 let s = get_state () in
 s.font <- f

let set_text_size sz =
  let s = get_state () in
  s.text_size <- sz

let text_size cs =
  let s = get_state () in
  let m = s.context##measureText (Js.string cs) in
  let dx = m##width in (* TODO check !!! *)
  (int_of_float dx, 10) (* TODO: fix height ? *)

(* Images *)

type image

let transp = -1

let make_image img = failwith "Graphics.make_image not implemented"
let dump_image img = failwith "Graphics.dump_image not implemented"
let draw_image img x y = failwith "Graphics.draw_image not implemented"
let create_image dx dy = failwith "Graphics.create_image not implemented"
let blit_image img x y = failwith "Graphics.blit_image not implemented"

let get_image x y w h =
  let image = create_image w h in
  blit_image image x y;
  image

(* Events *)

type status =
  { mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char }

type event =
    Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll

(*external wait_next_event : event list -> status = "caml_gr_wait_event" *)
let wait_next_event elist =
  failwith "Graphics.wait_next_event cannot be implemented"

let mouse_pos () =
  let e = wait_next_event [Poll] in (e.mouse_x, e.mouse_y)

let button_down () =
  let e = wait_next_event [Poll] in e.button

let read_key () =
  let e = wait_next_event [Key_pressed] in e.key

let key_pressed () =
  let e = wait_next_event [Poll] in e.keypressed

(*** Sound *)

let sound _ _ =
  failwith "Graphics.sound not implemented"

(* Splines *)
let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
and sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
and middle (x1, y1) (x2, y2) = ((x1 +. x2) /. 2.0,  (y1 +. y2) /. 2.0)
and area (x1, y1) (x2, y2) = abs_float (x1 *. y2 -. x2 *. y1)
and norm (x1, y1) = sqrt (x1 *. x1 +. y1 *. y1);;

let test a b c d =
 let v = sub d a in
 let s = norm v in
 area v (sub a b) <= s && area v (sub a c) <= s;;

let spline a b c d =
  let rec spl accu a b c d =
   if test a b c d then d :: accu else
   let a' = middle a b
   and o = middle b c in
   let b' = middle a' o
   and d' = middle c d in
   let c' = middle o d' in
   let i = middle b' c' in
   spl  (spl accu a a' b' i) i c' d' d in
  spl [a] a b c d;;

let curveto b c (x, y as d) =
 let float_point (x, y) = (float_of_int x, float_of_int y) in
 let round f = int_of_float (f +. 0.5) in
 let int_point (x, y) = (round x, round y) in
 let points =
   spline
    (float_point (current_point ()))
    (float_point b) (float_point c) (float_point d) in
 draw_poly_line
  (Array.of_list (List.map int_point points));
 moveto x y;;


(*
val close_graph : (unit -> unit)
*)
let close_graph () =
  match !state with
    None -> ()
  | Some c ->
    state := None;
    Utils.set_div_by_id "graphics" ""

let open_graph string =
  close_graph ();
  let canvas = Dom_html.createCanvas doc in
  let body = Utils.get_element_by_id "graphics" in
  let context = canvas##getContext (Dom_html._2d_) in
  Dom.appendChild body canvas;
  let x = 0 in
  let y = 0 in
  let width = 400 in
  let height = 400 in
  let color = blue in
  let line_width = 1 in
  let font = "fixed" in
  let text_size = 26 in
  state := Some { context; x; y; width; height; color;
                  line_width; font; text_size  }
