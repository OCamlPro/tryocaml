
let graphics_title_id = "graphics-title"
let graphics_id = "graphics"

exception Graphic_failure of string

(* NOT IMPLEMENTED:

- display_mode
- remember_mode
- synchronize

- draw_arc
- fill_arc

- events (button, keys)

- sound
*)

open Utils

type state = {
  context : Dom_html.canvasRenderingContext2D Js.t;
  canvas : Dom_html.canvasElement Js.t;
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
let graphics_title = ref "Graphics Window"

let get_state () =
  match !state with
    None -> raise (Graphic_failure "Not initialized")
  | Some s ->
(*
    Printf.fprintf stderr "state pos %d %d\n" s.x s.y;
    Printf.fprintf stderr "state screen %d %d\n" s.width s.height;
    Printf.fprintf stderr "state color %d\n" s.color;
*)
    s

let raw_set_color color =
(* TODO : do better ! *)
  let s = get_state () in
  s.color <- color;
  let color = Printf.sprintf "#%02x%02x%02x"
       ( (s.color lsr 16) land 0xff )
       ( (s.color lsr 8) land 0xff )
       ( (s.color lsr 0) land 0xff )
  in
  s.context##fillStyle <- Js.string color;
  ()

let raw_set_line_width w =
  let s = get_state () in
  s.line_width <- w;
  s.context##lineWidth <- float w

let raw_fill_rectangle context x1 y1 x2 y2 =
  let dx = x1 - x2 in
  let dy = y1 - y2 in
  let dx = if dx < 0 then -dx else dx in
  let dy = if dy < 0 then -dy else dy in
  let x1 = if x1 > x2 then x2 else x1 in
  let y1 = if y1 > y2 then y2 else y1 in
  context##fillRect (float x1, float y1, float dx, float dy)



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
  graphics_title := title;
  Utils.set_div_by_id graphics_title_id !graphics_title;
  (* TODO *) ()

let resize_window width height =
  let s = get_state () in
  s.width <- width;
  s.height <- height;
  s.canvas##width <- width;
  s.canvas##height <- height

let clear_graph () =
  let s = get_state () in
  s.canvas##width <- s.width;
  s.canvas##height <- s.height;
  s.context##strokeRect (0., 0., float s.width, float s.height);
  ()

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
  raw_set_color color

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
  let context = s.context in
  context##beginPath();
  context##moveTo (float x, float (s.height - y));
  context##lineTo (float x +. 1., float (s.height - y) +. 1.);
  context##stroke();
  ()

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
  let context = s.context in
  context##beginPath();
  context##moveTo (float s.x, float (s.height - s.y));
  context##lineTo (float x, float (s.height - y));
  context##stroke();
  s.x <- x;
  s.y <- y;
  ()


let rlineto x y = lineto (current_x () + x) (current_y () + y)
let rmoveto x y = moveto (current_x () + x) (current_y () + y)

let raw_draw_rect x y dx dy =
  let s = get_state () in
  s.context##strokeRect (float x, float (s.height - y), float dx, float (-dy))

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

let set_line_width w =
  if w < 0 then raise (Invalid_argument "set_line_width")
  else raw_set_line_width w
;;

let raw_fill_rect x y dx dy =
  let s = get_state () in
  s.context##fillRect (float x, float (s.height - y), float dx, float (-dy))

let fill_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "fill_rect")
  else raw_fill_rect x y w h
;;

let fill_poly point_array =
  let s = get_state () in
  s.context##beginPath();
  let p0 = point_array.(0) in
  s.context##moveTo (float (fst p0), float (s.height - snd p0));
  for i = 1 to Array.length point_array - 1 do
    let p1 = point_array.(i) in
    s.context##lineTo (float (fst p1), float (s.height - snd p1));
  done;
  s.context##lineTo (float (fst p0), float (s.height - snd p0));
  s.context##fill();
  ()

let raw_fill_arc x y rx ry a1 a2 =
  failwith "Graphics.raw_fill_arc not implemented"

let fill_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "fill_arc/ellipse/circle")
  else raw_fill_arc x y rx ry a1 a2
;;

let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360
let fill_circle x y r = fill_arc x y r r 0 360

(* Text *)


let text_size cs =
  let s = get_state () in
  let m = s.context##measureText (Js.string cs) in
  let dx = m##width in (* TODO check !!! *)
  (int_of_float dx, s.text_size) (* TODO: fix height ? *)

let draw_string cs =
  let s = get_state () in
  let m = s.context##measureText (Js.string cs) in
  let dx = m##width in
  s.context##fillText
    (Js.string cs, float s.x, float (s.height - s.y));
  s.x <- s.x + int_of_float dx

(*
  let m = s.context##measureText (Js.string cs) in
  let dx = m##width in (* TODO check !!! *)
*)
let draw_char c =
  let cs = String.make 1 c in draw_string cs

let set_font f =
 let s = get_state () in
 s.font <- f;
  s.context##font <- Js.string (Printf.sprintf "%dpx %s" s.text_size s.font)

let set_text_size sz =
  let s = get_state () in
  s.text_size <- sz;
  s.context##font <- Js.string (Printf.sprintf "%dpx %s" s.text_size s.font)

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


let loop_on_exit elist f =
  let doc = Dom_html.document in
  let canvas = (get_state ()).canvas in
  let cx, cy = canvas##offsetLeft, canvas##offsetTop in
  let button = ref false in
  let null = char_of_int 0 in
  let mouse_x, mouse_y = ref 0, ref 0 in

  let get_pos_mouse () = !mouse_x, !mouse_y in

  if List.mem Button_down elist then
    canvas##onmousedown <- Dom_html.handler (fun ev ->
      let mouse_x, mouse_y = get_pos_mouse () in
      button := true;
      let s = { mouse_x ; mouse_y ; button=true ;
		keypressed=false ; key=null } in
      f s;
      Js._true);

  if List.mem Button_up elist then
    canvas##onmouseup <- Dom_html.handler (fun ev ->
      let mouse_x, mouse_y = get_pos_mouse () in
      button := false;
      let s = { mouse_x ; mouse_y ; button=false ;
		keypressed=false ; key=null } in
      f s;
      Js._true);
  

  canvas##onmousemove <- Dom_html.handler (fun ev ->
    let state = get_state () in
    mouse_x := (Js.Optdef.get (ev##pageX) (fun _ -> 0)) - cx;
    mouse_y := state.height - (Js.Optdef.get (ev##pageY) (fun _ -> 0) - cy);
    if List.mem Mouse_motion elist then
      (let mouse_x, mouse_y = get_pos_mouse () in
       let s = { mouse_x ; mouse_y ; button=(!button) ;
		 keypressed=false ; key=null } in
       f s);
    Js._true);

  (* EventListener sur le doc car pas de moyen simple de le faire
     sur un canvasElement *)
  if List.mem Key_pressed elist then
    doc##onkeypress <- Dom_html.handler (fun ev ->
      (* Uncaught Invalid_argument char_of_int with key € for example *)
      let key =
	try char_of_int (Js.Optdef.get (ev##charCode) (fun _ -> 0))
	with Invalid_argument _ -> null in
      let mouse_x, mouse_y = get_pos_mouse () in
      let s = { mouse_x ; mouse_y ; button=(!button) ;
		keypressed=true ; key } in	
      f s;
      Js._true)


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
    let canvas = c.canvas in
    canvas##width <- 0;
    canvas##height <- 0;
    Utils.set_div_by_id graphics_id "";
    Utils.set_div_by_id graphics_title_id ""

let open_graph string =

  let size = ref "" in

  (* Parses the "command line" to determine whether or not a new window needs to
     be used as canvas *)
  let no_info, new_window = 
    try
      let sep = 
        try 
          String.index string ' '
        with _ -> 
          (* If the string begins with a number, we assume there is no display
             information *)
          let c = int_of_char string.[0] in
          if c >= 48 && c <= 57 then
            raise (Invalid_argument "No display information")
          else String.length string
      in 
      let display = String.create sep in
      String.blit string 0 display 0 sep;
      let l = (String.length string) - sep in
      size := String.sub string (sep+1) (l-1);
      false, not (display = "toplvl")
    with
        _ -> true, true
  in
    
  close_graph ();
  
  
  let x = 0 in
  let y = 0 in

  (* Parses the "command line" to find the size informations, returns 400x400 if
  it fails *)
  let width, height =
    try
      begin
        (* In case the user forgot to add the empty space before declaring size *)
        let size = if no_info then string else !size in 
        let sep = String.index size 'x' in
        let width = String.sub size 0 sep in
        let l = (String.length size) - sep - 1 in
        let sec_sep =
          try 
            (String.index size '+') - sep - 1
          with _ -> l
        in
        let height = String.sub size (sep+1) sec_sep in
        int_of_string width, int_of_string height
      end
    with
        _ -> 400, 400
  in

    (* If a new window is specified, will create a popup and return its document
  otherwise, it returns the actual document *)
  let doc = 
    if new_window then
      begin
        let params = 
          Format.sprintf "status=1,width=%d,height=%d" (width+20) (height+20) 
        in
        let params = Js.some (Js.string params) in
        let pop = 
          Dom_html.window##open_(
            Js.string "", 
            Js.string "OCaml Graphic context", 
            params)
        in
        pop##document
      end
    else doc
  in
  
  let canvas = Dom_html.createCanvas doc in
  let body = if new_window then doc##body
    else Utils.get_element_by_id "graphics" in
  let context = canvas##getContext (Dom_html._2d_) in
  Dom.appendChild body canvas;
  let x = 0 in
  let y = 0 in
  let color = blue in
  let line_width = 1 in
  let font = "fixed" in
  let text_size = 26 in
  let s =  { canvas; context; x; y; width; height; color;
                  line_width; font; text_size  }
  in
  state := Some s;
  clear_graph ();
  raw_set_color blue;
  set_text_size text_size;
  raw_set_line_width line_width;
  if not new_window then
    Utils.set_div_by_id graphics_title_id !graphics_title;
  ()
