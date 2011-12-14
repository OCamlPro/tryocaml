(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * (C) 2011 Jérôme Vouillon Laboratoire PPS - CNRS Université Paris Diderot
 * (C) 2011 Cagdas Bozman - OCamlPro SAS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

(****)

external global_data : unit -> Obj.t array = "caml_get_global_data"

let g = global_data ()

let _ =
  let toc = Obj.magic (Array.unsafe_get g (-2)) in
  let prims = split_primitives (List.assoc "PRIM" toc) in

  let compile s =
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Buffer.contents b
  in
  Array.unsafe_set g (-3) (Obj.repr compile); (*XXX HACK!*)

module Html = Dom_html

let s = ""

let doc = Html.document
let button_type = Js.string "reset"
let button txt action =
  let b = Dom_html.createInput ~_type:button_type doc in
  b##value <- Js.string txt;
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b

let exec ppf s =
  let lb = Lexing.from_string s in
  try
    List.iter
      (fun phr ->
        if not (Toploop.execute_phrase false ppf phr) then raise Exit)
      (!Toploop.parse_use_file lb)
  with
    | Exit -> ()
    | x    -> Errors.report_error ppf x

let start ppf =
  Format.fprintf ppf "        Try OCaml (v. %s)@.@." Sys.ocaml_version;
  Format.fprintf ppf "Hi ! How are you ? Welcome to TryOCaml. Let'start with your name ?\nType it with quotes around it like this \"Cagdas\"\n";
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "";
  exec ppf "open Tutorial"

let at_bol = ref true
let consume_nl = ref false

let refill_lexbuf s p ppf buffer len =
  if !consume_nl then begin
    let l = String.length s in
    if (!p < l && s.[!p] = '\n') then
      incr p
    else if (!p + 1 < l && s.[!p] = '\r' && s.[!p + 1] = '\n') then
      p := !p + 2;
    consume_nl := false
  end;
  if !p = String.length s then
    0
  else begin
    let c = s.[!p] in
    incr p;
    buffer.[0] <- c;
    if !at_bol then Format.fprintf ppf "# ";
    at_bol := (c = '\n');
    if c = '\n' then
      Format.fprintf ppf "@."
    else
      Format.fprintf ppf "%c" c;
    1
  end

let ensure_at_bol ppf =
  if not !at_bol then begin
    Format.fprintf ppf "@.";
    consume_nl := true; at_bol := true
  end
      
let loop s ppf =
  (* XXX use a regexp instead of substring *)
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) ppf) in
  begin try
    while true do
      try
        let phr = !Toploop.parse_toplevel_phrase lb in
        ensure_at_bol ppf;
        ignore (Toploop.execute_phrase true ppf phr)
      with
          End_of_file ->
            raise End_of_file
        | x ->
          ensure_at_bol ppf;
          Errors.report_error ppf x
    done
    with End_of_file -> ()
  end

let run _ =
  let top = 
    Js.Opt.get (doc##getElementById (Js.string "toplevel")) (fun () -> assert false) in
  let output = Html.createDiv doc in
  output##id <- Js.string "output";
  output##style##whiteSpace <- Js.string "pre";

  let ppf =
    Format.make_formatter
      (fun s i l ->
         Dom.appendChild output
           (doc##createTextNode(Js.string (String.sub s i l))))
      (fun _ -> ())
  in
  let textbox = Html.createTextarea doc in
  textbox##rows <- 7; 
  textbox##cols <- 70;
  textbox##value <- Js.string s;
  Dom.appendChild top textbox;
  textbox##focus();
  textbox##select();
  let container = 
    Js.Opt.get (doc##getElementById (Js.string "container")) (fun () -> assert false) in
  let output_area =
    Js.Opt.get (doc##getElementById (Js.string "output-area")) (fun () -> assert false) in
  
  let enter_key = 13 (* Enter key *) in
  Html.document##onkeyup <-
    (Html.handler
       (fun e ->
         if e##keyCode = enter_key then begin
           let str = textbox##value in
           textbox##value <- Js.string "";
           loop (Js.to_string str) ppf;
           textbox##focus();
           container##scrollTop <- container##scrollHeight;
           Js._false
         end        
         else Js._true));
  (* let b = *)
  (*   button "Reset" *)
  (*     (fun () -> *)
  (*       ()                              (\* TODO xxx clear all the div ? *\) *)
  (*     ) *)
  (* in *)
  output_area##scrollTop <- output_area##scrollHeight;
  (* let tryocaml =  *)
  (*   Js.Opt.get (doc##getElementById (Js.string "tryocaml")) (fun () -> assert false) in *)
  (* Dom.appendChild tryocaml b; *)
  Dom.appendChild output_area output;
  start ppf;
  Js._false
  
let _ = Html.window##onload <- Html.handler run
