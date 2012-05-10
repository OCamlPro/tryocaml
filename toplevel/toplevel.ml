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

open Utils

module Html = Dom_html

let init_in_lesson = ref (fun _ -> ())

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

class type global_data = object
  method toc : (string * string) list Js.readonly_prop
  method compile : (string -> string) Js.writeonly_prop
end

external global_data : unit -> global_data Js.t = "caml_get_global_data"

let g = global_data ()

let _ =
  let toc = g##toc in
  let prims = split_primitives (List.assoc "PRIM" toc) in

  let compile s =
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Buffer.contents b
  in
  g##compile <- compile (*XXX HACK!*)

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

let _ =
  Toploop.set_exn_printer 
    (fun _ _ exn -> 
      if Js.instanceof (Obj.magic exn) Js.array_empty then 
        raise Not_found
      else 
        Outcometree.Oval_printer (fun fmt ->          
          Format.fprintf fmt "%s"
            (Js.to_string 
               (Js.Unsafe.meth_call (Obj.magic exn) "toString" [||]))
        )
    )

let start ppf =
  Format.fprintf ppf "        Welcome to TryOCaml (v. %s)@.@." Sys.ocaml_version;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "";
  List.iter (fun s ->
    try
      exec ppf s
    with e ->
      Printf.printf "Exception %s while processing [%s]\n%!" (Printexc.to_string e) s
  )  [
    "Toploop.set_wrap true";
    "open Tutorial";
    "#install_printer Toploop.print_hashtbl";
    "#install_printer Toploop.print_queue";
    "#install_printer Toploop.print_stack";
    "#install_printer Toploop.print_lazy";
    "#install_printer N.print";

(* for Num/Big_int: *)
    "#install_printer Topnum.print_big_int";
    "#install_printer Topnum.print_num";
    "#install_printer Toploop.print_exn";    
    "open Topnum";
  ];
  ()

let at_bol = ref true
let consume_nl = ref false

let input = ref []
let output = ref []

let rec refill_lexbuf s p ppf buffer len =
  match !input with
    | '\000' :: tail ->
      input := tail;
      refill_lexbuf s p ppf buffer len
    | c :: tail ->
      input := tail;
      output := c :: !output;
      buffer.[0] <- c;
      1
    | [] ->
      if !consume_nl then begin
        let l = String.length s in
        if (!p < l && s.[!p] = '\n') then
          incr p
        else if (!p + 1 < l && s.[!p] = '\r' && s.[!p + 1] = '\n') then
          p := !p + 2;
        consume_nl := false
      end;
      if !p = String.length s then begin
        output := '\000' :: !output;
        0
      end else begin
        let c = s.[!p] in
        incr p;
        buffer.[0] <- c;
        if !at_bol then Format.fprintf ppf "# ";
        at_bol := (c = '\n');
        if c = '\n' then
          Format.fprintf ppf "@."
        else
          Format.fprintf ppf "%c" c;
        output := c :: !output;
        1
      end

let ensure_at_bol ppf =
  if not !at_bol then begin
    Format.fprintf ppf "@.";
    consume_nl := true; at_bol := true
  end

let update_lesson_text () =
  if !Tutorial.this_lesson <> 0 then begin
    !init_in_lesson ();
    set_div_by_id "lesson-text" !Tutorial.this_step_html
  end
    
let update_lesson_number () =
  if  !Tutorial.this_lesson <> 0 then
    set_div_by_id "lesson-number"
      (Printf.sprintf "<span class=\"lesson\">%s %d</span>"
         (Tutorial.translate "Lesson")
         !Tutorial.this_lesson)
      
let update_lesson_step_number () =
  if !Tutorial.this_lesson <> 0 then
    set_div_by_id "lesson-step"
      (Printf.sprintf "<span class=\"step\">%s %d</span>"
         (Tutorial.translate "Step")
         !Tutorial.this_step)

let update_prompt prompt =
  set_div_by_id "sharp" prompt

let extract_escaped_and_kill html i =
  let len = String.length html in
  let rec iter html i len =
    if i = len then i else
      match html.[i] with
          ';' -> i+1
        | _ -> iter html (i+1) len
  in
  let end_pos = iter html (i+1) len in
  let s = String.sub html i (end_pos - i) in
  for j = i to end_pos - 1 do
    html.[j] <- '\000'
  done;
  s

let text_of_html html =
  let b = Buffer.create (String.length html) in
  for i = 0 to String.length html - 1 do
    match html.[i] with
        '&' ->
          begin
            match extract_escaped_and_kill html i with
              | "&gt;" -> Buffer.add_char b '>'
              | "&lt;" -> Buffer.add_char b '<'
              | "&amp;" -> Buffer.add_char b '&'
              | _ -> ()
          end
      | '\000' -> ()
      | c -> Buffer.add_char b c
  done;
  Buffer.contents b

exception End_of_input

let string_of_char_list list =
  let len = List.length list in
  let s = String.create len in
  let rec iter s i list =
    match list with
        [] -> s
      | c :: tail ->
        s.[i] <- c;
        iter s (i+1) tail
  in
  iter s 0 list

let loop s ppf buffer =
  let s =
    if !Tutorial.use_multiline then begin
      input := List.rev ('\n' :: !output);
      output := [];
      s
    end else begin
      let need_terminator = ref true in
      for i = 0 to String.length s - 2 do
        if s.[i] = ';' && s.[i+1] = ';' then need_terminator := false;
      done;
      output := [];
      if !need_terminator then s ^ ";;" else s
    end
  in
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) ppf) in
  begin try
    while true do
      begin
      try
        let phr = try
                    !Toploop.parse_toplevel_phrase lb
          with End_of_file -> raise End_of_input
            | e ->
              let input = string_of_char_list (List.rev !output) in
              Tutorial.print_debug (Printf.sprintf "debug: input = [%s]"  (String.escaped input));
              raise e
        in
        let input = string_of_char_list (List.rev !output) in
        Tutorial.print_debug (Printf.sprintf "debug: input = [%s]"  (String.escaped input));
        if !Tutorial.use_multiline then begin
          match !output with
              ';' :: ';' :: _ -> output := []
            | _ -> assert false
        end else output := [];
        ensure_at_bol ppf;
        Buffer.clear buffer;
        Tutorial.print_debug s;
        ignore (Toploop.execute_phrase true ppf phr);
        let res = Buffer.contents buffer in
        Tutorial.check_step ppf input res;
        update_lesson_text ();
        update_lesson_number ();
        update_lesson_step_number ();
      with
          End_of_input ->
            ensure_at_bol ppf;
            raise End_of_input
        | x ->
          let do_report_error =
            if !Tutorial.use_multiline then
              match !output with
                  '\000' :: _ -> false
                | _ -> true
            else true
          in
          if do_report_error then begin
            output := [];
            ensure_at_bol ppf;
            Errors.report_error ppf x
          end
      end;
    done
    with End_of_input ->
      match !output with
          [] | [ '\000' ] ->
            output := []; update_prompt "#"
        | _ ->
          let s = string_of_char_list (List.rev !output) in
          let len = String.length s in
          let s = if len >= 5 then String.sub s 0 5 else s in
          update_prompt (Printf.sprintf "[%s]> " s)
  end

let _ =
  Tutorial.message_fun := (fun s ->
    if  !Tutorial.this_lesson <> 0 then
      set_div_by_id "lesson-message"
        (Printf.sprintf
           "<div class=\"alert-message block-message success\">%s</div>" s)
  )

let to_update = [
  "main-title", "Try OCaml";

  "short-intro",
  "OCaml is a strongly typed functional language. It is concise and fast, enabling you to improve your coding efficiency while producing code with higher quality.";

  "text-commands", "Commands";
  "text-effects", "Effects";
  "text-enter", "Enter / Return";
  "text-submit", "Submit code";
  "text-arrows", "Up / Down";
  "text-history", "Cycle through history";
  "text-newline", "Shift + Enter";
  "text-multiline",  "Multiline edition";
  "text-lesson-1", "Move to lesson 1";
  "text-step-1", "Move to step 1 of the current lesson";
  "text-lessons", "See available lessons";
  "text-steps",	"See available steps in the current lesson";
  "text-next", "Move to the next step";
  "text-back", "Move to the previous step";
]

let _ =
  Tutorial.update_lang_fun := (fun _ ->
    List.iter (fun list ->
      List.iter (fun (id, s) ->
        set_div_by_id id (Tutorial.translate s))
        list)
      [ to_update; !Button.registered_buttons ]
  )

let get_storage () = 
  match Js.Optdef.to_option window##localStorage with
      None -> assert false
    | Some t -> t

let get_history_size () =
  let st = get_storage () in
  match Js.Opt.to_option
    (st##getItem(Js.string "history last")) with
      | None -> 0
      | Some s -> try int_of_string (Js.to_string s) with _ -> 0

let set_history_size i =
  let st = get_storage () in
  st##setItem(Js.string "history last",
                                Js.string (string_of_int i))

let get_history () =
  try
    let st = get_storage () in
    let size = get_history_size () in
    let h = Array.init size
      (fun i -> Js.Opt.get
        (st##getItem(
          Js.string (Printf.sprintf "history %i" i)))
        (fun () -> failwith "no history item")) in
    Array.to_list h
  with _ -> (* Probably no local storage *)
    []

let append_children id list =
  let ele = get_element_by_id id in
  List.iter (fun w -> Dom.appendChild ele w) list

let add_history s =
  try
    let st = get_storage () in
    let size = get_history_size () in
    st##setItem(
      Js.string (Printf.sprintf "history %i" size), s);
    set_history_size (size+1);
  with
    | _ -> Firebug.console##warn(Js.string "can't set history")

let run () =
  let top = get_element_by_id "toplevel"  in
  let output_area = get_element_by_id "output" in
  let buffer = Buffer.create 1000 in
  let ppf =
    let b = Buffer.create 80 in
    Format.make_formatter
      (fun s i l ->
        Buffer.add_substring buffer s i l;
        Buffer.add_substring b s i l)
      (fun _ ->
        Dom.appendChild output_area
          (doc##createTextNode(Js.string (Buffer.contents b)));
        Buffer.clear b) in

  let textbox = Html.createTextarea doc in
  textbox##value <- Js.string "";
  textbox##id <- Js.string "console";
  Dom.appendChild top textbox;
  textbox##focus();
  textbox##select();

  let container = get_element_by_id "toplevel-container" in
  let history = ref (get_history ()) in
  let history_bckwrd = ref !history in
  let history_frwrd = ref [] in

  let rec make_code_clickable () =
    let textbox = get_element_by_id "console" in
    let textbox = match Js.Opt.to_option (Html.CoerceTo.textarea textbox) with
      | None   -> assert false
      | Some t -> t in
    let codes = Dom.list_of_nodeList (doc##getElementsByTagName(Js.string "code")) in
    List.iter (fun code ->
      let html =  code##innerHTML in
      let txt = text_of_html (Js.to_string html) in
      code##title <- Js.string (Tutorial.translate "Click here to execute this code");
      code##onclick <- Html.handler (fun _ ->
        textbox##value <- Js.string ( txt ^ ";;" );
        execute ();
        Js._true)
    ) codes
      
  and execute () =
    let s = Js.to_string textbox##value in
    if s <> "" then
      begin
        history := Js.string s :: !history;
        add_history (Js.string s);
      end;
    history_bckwrd := !history;
    history_frwrd := [];
    textbox##value <- Js.string "";
    (try loop s ppf buffer with _ -> ());
    Tutorial.debug_fun := (fun s -> Firebug.console##log (Js.string s));
    make_code_clickable ();
    textbox##focus();
    container##scrollTop <- container##scrollHeight 
  in 
  container##onclick <- Html.handler 
    (fun _ ->
      textbox##focus();  textbox##select();  Js._true);

  (* Start drag and drop part *)
  let ev = DragnDrop.init () in  
  (* Customize dropable part *)
  ev.DragnDrop.ondrop <- (fun e  ->
    container##className <- Js.string "";
    let file =
      match Js.Opt.to_option (e##dataTransfer##files##item(0)) with
        | None -> assert false
        | Some file -> file in

    let reader = jsnew File.fileReader () in    
    reader##onload <- Dom.handler
      (fun _ ->
        let s =
          match Js.Opt.to_option (File.CoerceTo.string (reader##result)) with
            | None -> assert false
            | Some str -> str
        in
        textbox##value <- s;
        execute ();
        textbox##value <- Js.string "";
        Js._false);
    reader##onerror <- Dom.handler
      (fun _ ->
        Firebug.console##log (Js.string "Drang and drop failed.");
        textbox##value <- Js.string "Printf.printf \"Drag and drop failed. Try again\"";
        execute ();
        textbox##value <- Js.string "";
        Js._true);
    reader##readAsText ((file :> (File.blob Js.t)));
    Js._false);
  DragnDrop.make ~events:ev container;
  (* End of Drag and drop part *)
  
  let tbox_init_size = textbox##style##height in
  Html.document##onkeydown <-
    (Html.handler
       (fun e -> match e##keyCode with
         | 13 -> (* ENTER key *)
           let keyEv = match Js.Opt.to_option (Html.CoerceTo.keyboardEvent e) with
             | None   -> assert false
             | Some t -> t in
           (* Special handling of ctrl key *)
           if keyEv##ctrlKey = Js._true then
             textbox##value <- Js.string ((Js.to_string textbox##value) ^ "\n");
           if keyEv##ctrlKey = Js._true || keyEv##shiftKey = Js._true then
             let rows_height = textbox##scrollHeight / (textbox##rows + 1) in
             let h = string_of_int (rows_height * (textbox##rows + 1) + 20) ^ "px" in
             textbox##style##height <- Js.string h;
             Js._true
           else begin
             execute ();
             textbox##style##height <- tbox_init_size;
             textbox##value <- Js.string "";
             Js._false
           end
	 | 38 -> (* UP ARROW key *) begin
	   match !history_bckwrd with
	     | s :: l ->
	       let str = Js.to_string textbox##value in
	       history_frwrd := Js.string str :: !history_frwrd;
	       textbox##value <- s;
	       history_bckwrd := l;
	       Js._false
	     | _ -> Js._true
	 end
	 | 40 -> (* DOWN ARROW key *) begin
	   match !history_frwrd with
	     | s :: l ->
	       let str = Js.to_string textbox##value in
	       history_bckwrd := Js.string str :: !history_bckwrd;
	       textbox##value <- s;
	       history_frwrd := l;
	       Js._false
	     | _ -> Js._true
	 end
	 | _ -> Js._true));
  
  let clear () = 
    output_area##innerHTML <- (Js.string "");
    textbox##focus();
    textbox##select() in

  let reset () =
    output_area##innerHTML <- (Js.string "");
    Toploop.initialize_toplevel_env ();
    Toploop.input_name := "";
    exec ppf "open Tutorial";
    textbox##focus();
    textbox##select() in
  let set_cols i = 
    textbox##style##width <- Js.string ((string_of_int (i * 7)) ^ "px") in

  let send_button = 
    Button.create (Tutorial.translate "Send") (fun () -> execute ()) in
  let clear_button = 
    Button.create 
      (Tutorial.translate "Clear") (fun () -> clear ()) in
  let reset_button = 
    Button.create (Tutorial.translate "Reset") (fun () -> reset ()) in
  let save_button =  
    Button.create (Tutorial.translate "Save") (fun () ->
    let content = Js.to_string output_area##innerHTML in
    let l = Regexp.split (Regexp.regexp ("\n")) content in
    let content =
      Js.string (
        let l = List.filter (fun x ->
          try x.[0] = '#' with _ -> false) l in
        let l = List.map  (fun x -> String.sub x 2 ((String.length x) - 2)) l in
        String.concat "\n" l)
    in
    let uriContent =
      Js.string ("data:application/octet-stream," ^
                    (Js.to_string (Js.encodeURI content))) in
    let _ = window##open_(uriContent, Js.string "Try OCaml", Js.null) in
    window##close ()) in
  
  let update_lesson () =
    update_lesson_number ();
    update_lesson_step_number ();
    update_lesson_text ();
    make_code_clickable ();
    Cookie.set_cookie "lang" (Tutorial.lang ());
    Cookie.set_cookie "lesson" (string_of_int !Tutorial.this_lesson);
    Cookie.set_cookie "step" (string_of_int !Tutorial.this_step)
  in
  
  (* Choose your language *)
  let form = Html.createDiv doc in
  let sel = Dom_html.createSelect doc in
  sel##id <- Js.string "languages";
  List.iter (fun (_, (lang, _)) ->
    let opt = Html.createOption doc in
    Dom.appendChild opt (doc##createTextNode (Js.string lang));
    sel##add (opt, Js.null);
  ) Tutorial.langs;
  sel##onchange <-
    Html.handler
    (fun _ ->
      Tutorial.set_lang (fst (List.nth Tutorial.langs sel##selectedIndex));
      Cookie.set_cookie "lang" (Tutorial.lang ());
      update_lesson ();
      Js._true);
  Dom.appendChild form sel;
  let langs = get_element_by_id "languages" in
  Dom.appendChild langs form;

  set_cols 80;
  append_children "buttons" [
    send_button; clear_button; reset_button; save_button];

  (* Choice of lesson and step with URL *)
  let update_lesson_step lesson step =
    Tutorial.lesson lesson;
    Tutorial.step step;
    update_lesson () in

  init_in_lesson :=
    (let init = ref false in
     fun () ->
       if not !init then begin
         init := true;

         append_children "lesson-left-button" [
           Button.create_with_image "images/left2.png" 16 (Tutorial.translate "left2")
             (fun _ ->
               Tutorial.lesson (!Tutorial.this_lesson-1);
               update_lesson ();
             );
         ];
         append_children "lesson-right-button" [
           Button.create_with_image "images/right2.png" 16 (Tutorial.translate "right2")
             (fun _ ->
               Tutorial.lesson (!Tutorial.this_lesson+1);
               update_lesson ();
             );
         ];
         append_children "step-left-button" [
           Button.create_with_image "images/left1.png" 16 (Tutorial.translate"left1")
             (fun _ ->
               Tutorial.back();
             update_lesson ();
           );
         ];
         append_children "step-right-button" [
           Button.create_with_image "images/right1.png" 16 (Tutorial.translate "right1")
             (fun _ ->
             Tutorial.next();
             update_lesson ();
           );
         ];
       end);

  output_area##scrollTop <- output_area##scrollHeight;
  make_code_clickable ();
  clear ();
  start ppf;

  (* Function to handle cookie operations *)
  let get_lang_from_cookie () =
    let default_lang = "en" in
    let cookie = Cookie.get_cookie () in
    try 
      snd (List.find (fun (key, value) -> key = "lang" ) cookie)
    with Not_found -> default_lang in
                
  let get_lesson_from_cookie () =
    let cookie = Cookie.get_cookie () in
    try 
      let _, lesson = List.find (fun (key, value) -> key = "lesson" ) cookie in
      int_of_string lesson
    with Not_found -> 0 in
  
  let get_step_from_cookie () =
    let cookie = Cookie.get_cookie () in
    try 
      let _, step = List.find (fun (key, value) -> key = "step" ) cookie in
      int_of_string step
    with Not_found -> 0 in
  
  let set_lang_from_cookie () =
    let lang = get_lang_from_cookie () in
    if lang <> "" then Tutorial.set_lang lang in

  let set_lesson_step_from_cookie () =
    let lesson = get_lesson_from_cookie () in
    let step = get_step_from_cookie () in
    update_lesson_step lesson step in
  
  (* Check if language has change in URL *)
  let url = Js.decodeURI loc##href in
  let reg = Regexp.regexp ".*lang=([a-z][a-z]).*" in
  let _ =
    match Regexp.string_match reg (Js.to_string url) 0 with
      | None -> set_lang_from_cookie ()
      | Some r ->
        match Regexp.matched_group r 1 with
            None -> set_lang_from_cookie ()
          | Some s ->
              Tutorial.set_lang s;
              Cookie.set_cookie "lang" (Tutorial.lang ()) in
  let reg_lesson = Regexp.regexp ".*lesson=([0-9]+).*" in
  let reg_step = Regexp.regexp ".*step=([0-9]+).*" in
  let _ =
    match Regexp.string_match reg_lesson (Js.to_string url) 0 with
      | None -> ()
      | Some r ->
        match Regexp.matched_group r 1 with
            None -> ()
          | Some s ->
              Cookie.set_cookie "lesson" s;
            Tutorial.lesson (int_of_string s) in
  let _ =
    match Regexp.string_match reg_step (Js.to_string url) 0 with
      | None -> set_lesson_step_from_cookie ()
      | Some r ->
        match Regexp.matched_group r 1 with
            None -> set_lesson_step_from_cookie ()
          | Some s ->
              Cookie.set_cookie "step" s;
              Tutorial.step (int_of_string s) in
  update_lesson_step !Tutorial.this_lesson !Tutorial.this_step;
  Js._false

let main () =
  try
    ignore (run ());
  with e ->
    window##alert (Js.string
                     (Printf.sprintf "exception %s during init."
                        (Printexc.to_string e)))

(* Force some dependencies to be linked : *)
let _ =
  Tutorial.init ();
  Topnum.init ();
  ()
