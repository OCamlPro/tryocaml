let debug = ref false

let debug_fun = ref (fun _ -> ())


let print_debug s = if !debug then (!debug_fun) s

let message_fun = ref (fun _ -> ())

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
    List.iter (fun (num, _, _, _) -> if num > !max_lesson then max_lesson := num) Lessons.lessons;
    let all_lessons = Array.create (!max_lesson+1) None in
    all_lessons
  in
  List.iter (fun (num, lesson_title, lesson_html, steps) ->
    let max_steps = ref 1 in
    List.iter (fun (num,_,_, _) -> if num > !max_steps then max_steps := num) steps;
    let all_steps = Array.create (!max_steps+1) None in
    List.iter (fun (num, step_title, step_html, step_check) ->
      all_steps.(num) <- Some (step_title, step_html, step_check)
    ) steps;
    all_lessons.(num) <- Some (lesson_title, lesson_html, all_steps)
  ) Lessons.lessons;
  all_lessons

let user_navigation = ref false

let rec step num =
  print_debug (Printf.sprintf "step %d\n" num);
  if num < 1 then lesson_back (!this_lesson - 1) else
    if num >= Array.length !this_lesson_steps then
      lesson (!this_lesson + 1)
    else
      match (!this_lesson_steps).(num) with
          None -> step (num + 1)
        | Some (step_title, step_html, step_check) ->
          user_navigation := true;
          this_step := num;
          this_step_title := step_title;
          this_step_html := step_html;
          this_step_check := step_check

and lesson num =
  print_debug (Printf.sprintf "lesson %d\n" num);
  if num >= 1 && num < Array.length lessons_table then
    match lessons_table.(num) with
        None -> lesson (num + 1)
      | Some (lesson_title, lesson_html, steps) ->
        this_lesson := num;
        this_lesson_title := lesson_title;
        this_lesson_html := lesson_html;
        this_lesson_steps := steps;
        step 1

and lesson_back num =
  print_debug (Printf.sprintf "lesson_back %d\n" num);
  if num >= 1 && num < Array.length lessons_table then
    match lessons_table.(num) with
        None -> lesson_back (num - 1)
      | Some (lesson_title, lesson_html, steps) ->
        this_lesson := num;
        this_lesson_steps := steps;
        this_lesson_title := lesson_title;
        this_lesson_html := lesson_html;
        step (Array.length steps - 1)

let check_step ppf input output =
  print_debug (Printf.sprintf  "debug: input=[%s] output=[%s]" (String.escaped input) (String.escaped output));
  if !user_navigation then begin
    user_navigation := false;
    (!message_fun) (Printf.sprintf "You moved to lesson %d, step %d." !this_lesson !this_step)
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
      (!message_fun) "Congratulations ! You moved to the next lesson."
    else
      if current_step < !this_step then
        (!message_fun) "Congratulations ! You moved to the next step."
  end else
    (!message_fun) ""

let next () = step (!this_step + 1)
let back () = step (!this_step - 1)

let debug d =
  debug := d;
  if not d then (!debug_fun "")

let clear_fun = ref (fun _ -> ())
let clear () = !clear_fun ()

let lessons () =
  Printf.printf "All lessons:\n%!";
  let left = ref true in
  for i = 0 to Array.length lessons_table - 1 do
    match lessons_table.(i) with
        None -> ()
      | Some (lesson_title, _, steps) ->
        if !left then
          Printf.printf "%2d   %-30s" i lesson_title
        else
          Printf.printf "%2d   %-30s\n" i lesson_title;
        left := not !left
  done;
  if not !left then Printf.printf "\n%!"

let steps () =
  Printf.printf "All steps in lesson %d:\n%!" !this_lesson;
  for i = 0 to Array.length !this_lesson_steps - 1 do
    match (!this_lesson_steps).(i) with
        None -> ()
      | Some (step_title, _, _) ->
        Printf.printf "%d\t%s\n%!" i step_title
  done

let use_multiline = ref false
let multiline flag = use_multiline := flag
