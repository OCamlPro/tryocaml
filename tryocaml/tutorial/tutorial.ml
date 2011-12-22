
let this_lesson = ref 1
let this_lesson_steps = ref [||]
let this_step = ref 1
let this_step_check = ref (fun _ _ -> false)
let this_step_txt = ref ""

let lessons =
  let all_lessons =
    let max_lesson = ref 1 in
    List.iter (fun (num, _) -> if num > !max_lesson then max_lesson := num) Lessons.lessons;
    let all_lessons = Array.create (!max_lesson+1) None in
    all_lessons
  in
  List.iter (fun (num, steps) ->
    let max_steps = ref 1 in
    List.iter (fun (num,_, _) -> if num > !max_steps then max_steps := num) steps;
    let all_steps = Array.create (!max_steps+1) None in
    List.iter (fun (num, step_txt, step_check) ->
      all_steps.(num) <- Some (step_txt, step_check)
    ) steps;
    all_lessons.(num) <- Some all_steps
  ) Lessons.lessons;
  all_lessons

let warning = ref true

let rec step num =
  if num < 1 then lesson (!this_lesson - 1) else
    if num >= Array.length !this_lesson_steps then
      lesson (!this_lesson + 1)
    else
      match (!this_lesson_steps).(num) with
          None -> step (num + 1)
        | Some (step_txt, step_check) ->
          warning := false;
          this_step := num;
          this_step_txt := step_txt;
          this_step_check := step_check

and lesson num =
  if num >= 1 && num < Array.length lessons then
    match lessons.(num) with
        None -> lesson (num + 1)
      | Some steps ->
        this_lesson := num;
        this_lesson_steps := steps;
        step 1
  else
    failwith "No such lesson"

let _ =  lesson 1

let debug = ref false

let check_step ppf input output =
  if !debug then
    Format.fprintf ppf "debug: input=[%s] output=[%s]@." (String.escaped input) (String.escaped output);
  let result =
    try (!this_step_check) input output with _ -> false
  in
  if result then begin
      Format.fprintf ppf "Congratulations ! You moved to the next step !@.";
      step (!this_step + 1)
  end else
    if !warning then
      Format.fprintf ppf "Try again...@."
    else
      Format.ifprintf Format.err_formatter "You have moved to lesson %d, step %d.@." !this_lesson !this_step;
  warning := true

let next () = step (!this_step + 1)
let back () = step (!this_step - 1)

let debug d = debug := d
