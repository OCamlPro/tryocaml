
let debug = ref false

let debug_fun = ref (fun _ -> ())
let update_lang_fun = ref (fun _ -> ())
let read_fun = ref (fun _ _ -> "")
let read_bool = fun () -> bool_of_string (!read_fun "Enter your answer" "")
let read_string = fun () -> !read_fun "Enter your answer" ""
let read_int = fun () -> int_of_string (!read_fun "Enter your answer" "")
let read_float = fun () -> float_of_string (!read_fun "Enter your answer" "")

let print_debug s = if !debug then (!debug_fun) s

let message_fun = ref (fun _ -> ())

let default_language = "en"
let default_translation = Hashtbl.create 13
let default_language_name = "English"
let _ = Hashtbl.add default_translation
  default_language_name default_language_name

let langs = ref [
  (default_language, (default_language_name, default_translation))
]

let _ =
  List.iter (fun (lang, messages) ->
    let h = Hashtbl.create 13 in
    List.iter (fun (x,y) -> Hashtbl.add h x y) messages;
    try
      let name = Hashtbl.find h default_language_name in
      langs := ( lang, (name,h) ) :: !langs
    with Not_found -> ()
  ) Lessons.langs

let langs = List.sort compare !langs

let current_lang = ref default_language
let current_translation = ref default_translation

let translate s =
  try Hashtbl.find !current_translation s with Not_found -> s

let this_lesson = ref 0
let this_lesson_title = ref ""
let this_lesson_html = ref ""

let this_lesson_steps = ref [||]
let this_step = ref 0
let this_step_check = ref (fun _ _ -> false)
let this_step_title = ref ""
let this_step_html = ref ""

let lessons_table =
  let all_lessons =
    let max_lesson = ref 1 in
    List.iter (fun (num, _, _, _, _) -> if num > !max_lesson then max_lesson := num) Lessons.lessons;
    let all_lessons = Array.create (!max_lesson+1) None in
    all_lessons
  in
  List.iter (fun (num, lesson_title, lesson_html, lesson_langs, steps) ->
    let max_steps = ref 1 in
    List.iter (fun (num,_,_,_, _) -> if num > !max_steps then max_steps := num) steps;
    let all_steps = Array.create (!max_steps+1) None in
    List.iter (fun (num, step_title, step_html, step_langs, step_check) ->
      all_steps.(num) <- Some (step_title, step_html, step_langs, step_check)
    ) steps;
    all_lessons.(num) <- Some (lesson_title, lesson_html, lesson_langs, all_steps)
  ) Lessons.lessons;
  all_lessons

let user_navigation = ref false

let get_lesson lesson_title lesson_html lesson_langs =
  try
    List.assoc !current_lang lesson_langs
  with Not_found -> (lesson_title, lesson_html)

let update_lesson () =
  match lessons_table.(!this_lesson) with
      None -> ()
    | Some (lesson_title, lesson_html, lesson_langs, steps) ->
      let (title, html) = get_lesson lesson_title lesson_html lesson_langs in
      this_lesson_title := title;
      this_lesson_html := html;
      this_lesson_steps := steps;
      ()

let get_step step_title step_html step_langs =
  try
    let (title, html) = List.assoc !current_lang step_langs in
    (title, html)
  with Not_found ->
    (step_title, step_html)

let update_step () =
  if !this_lesson_steps <> [||] then
  match (!this_lesson_steps).(!this_step) with
      None -> assert false
    | Some (step_title, step_html, step_langs, step_check) ->
      user_navigation := true;
      let (title, html) = get_step step_title step_html step_langs in
      this_step_title := title;
      this_step_html := html;
      this_step_check := step_check;
      ()

let rec step num =
  print_debug (Printf.sprintf "step %d\n" num);
  if num < 1 then lesson_back (!this_lesson - 1) else
    if num >= Array.length !this_lesson_steps then
      lesson (!this_lesson + 1)
    else
      match (!this_lesson_steps).(num) with
          None -> step (num + 1)
        | Some (step_title, step_html, step_langs, step_check) ->
          this_step := num;
          update_step ()

and lesson num =
  print_debug (Printf.sprintf "lesson %d\n" num);
  if num >= 1 && num < Array.length lessons_table then
    match lessons_table.(num) with
        None -> lesson (num + 1)
      | Some (lesson_title, lesson_html, lesson_langs, steps) ->
        this_lesson := num;
        update_lesson ();
        step 1

and lesson_back num =
  print_debug (Printf.sprintf "lesson_back %d\n" num);
  if num >= 1 && num < Array.length lessons_table then
    match lessons_table.(num) with
        None -> lesson_back (num - 1)
      | Some (lesson_title, lesson_html, lesson_langs, steps) ->
        this_lesson := num;
        update_lesson ();
        step (Array.length steps - 1)

let check_step ppf input output =
  print_debug (Printf.sprintf "debug: output=[%s]" (String.escaped output));
  if !user_navigation then begin
    user_navigation := false;
    (!message_fun) (Printf.sprintf "%s %d, %s %d."
                      (translate  "You moved to lesson")
                      !this_lesson
                      (translate "step")
                      !this_step)
  end else
  let result =
    try (!this_step_check) input output with _ -> false
  in
  if result then begin
    let current_lesson = !this_lesson in
    let current_step = !this_step in
    step (!this_step + 1);
    user_navigation := false;
    if current_lesson < !this_lesson then
      (!message_fun) (Printf.sprintf "%s ! %s."
                        (translate "Congratulations")
                        (translate "You moved to the next lesson"))
    else
      if current_step < !this_step then
        (!message_fun) (Printf.sprintf "%s ! %s."
                          (translate "Congratulations")
                          (translate "You moved to the next step"))
  end else
    (!message_fun) ""

let next () = step (!this_step + 1)
let back () = step (!this_step - 1)

let debug d =
  debug := d;
  if not d then (!debug_fun "")

let lessons () =
  Printf.printf "%s:\n%!" (translate "All lessons");
  let left = ref true in
  for i = 0 to Array.length lessons_table - 1 do
    match lessons_table.(i) with
        None -> ()
      | Some (lesson_title, lesson_html, lesson_langs, steps) ->
        let (title, html) = get_lesson lesson_title lesson_html lesson_langs in
        if !left then
          Printf.printf "%2d   %-30s" i title
        else
          Printf.printf "%2d   %-30s\n" i title;
        left := not !left
  done;
  if not !left then Printf.printf "\n%!"

let steps () =
  Printf.printf "%s %d:\n%!" (translate "All steps in lesson")!this_lesson;
  for i = 0 to Array.length !this_lesson_steps - 1 do
    match (!this_lesson_steps).(i) with
        None -> ()
      | Some (step_title, step_html, step_langs, _) ->
        let (title, html) = get_step step_title step_html step_langs in
        Printf.printf "%d\t%s\n%!" i title
  done

let use_multiline = ref false
let multiline flag = use_multiline := flag

let set_lang lang =
  let (_, translation) = List.assoc lang langs in
  current_lang := lang;
  current_translation := translation;
  update_lesson ();
  update_step ();
  !update_lang_fun ()

let lang () = !current_lang

external int_of_int : int -> int = "%identity"
external nativeint_of_nativeint : nativeint -> nativeint = "%identity"
external float_of_float : float -> float = "%identity"
external int32_of_int32 : int32 -> int32 = "%identity"
external int64_of_int64 : int64 -> int64 = "%identity"

external int_to_int : int -> int = "%identity"
external float_to_float : float -> float = "%identity"

module Tutorial = struct end

let init () =
  N.init ()
