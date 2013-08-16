let list_directory dirname =
  let dir = Unix.opendir dirname in
  let list = ref [] in
  try
    while true do
      let s = Unix.readdir dir in
      match s with
          "." | ".." -> ()
        | s -> list := s :: !list
    done;
    assert false
  with End_of_file ->
    Unix.closedir dir;
    !list

let string_of_file filename =
  let b = Buffer.create 1000 in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      Printf.bprintf b "%s\n" line
    done;
    assert false
  with End_of_file ->
    close_in ic;
    Buffer.contents b

let string_starts_with s prefix =
  let len_s = String.length s in
  let len_prefix = String.length prefix in
  if len_s >= len_prefix then
    let rec string_starts_with s prefix i len =
      if i = len then true else
        if s.[i] = prefix.[i] then string_starts_with s prefix (i+1) len
        else false
    in
    string_starts_with s prefix 0 len_prefix
  else
    false

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

let find_substring s to_find =
  let to_find_len = String.length to_find in
  let rec iter s i to_find to_find_len =
    if String.sub s i to_find_len = to_find then i
    else iter s (i+1) to_find to_find_len
  in
  iter s 0 to_find to_find_len

let get_title html =
  try
    let pos1 = find_substring html "<h3>" in
    let pos2 = find_substring html "</h3>" in
    String.sub html (pos1+4) (pos2-pos1-4)
  with e ->
    Printf.fprintf stderr "Warning: could not find title between <h3>...</h3>\n%!";
    raise e

type lesson = {
  lesson_num : int;
  lesson_text : text;
  mutable lesson_langs : (string * text) list;
  lesson_steps : step array;
}

and step = {
  step_num : int;
  step_text : text;
  mutable step_langs : (string * text) list;
  step_test : string;
}

and text = {
  title : string;
  html : string;
}

let lessons_dir = Sys.argv.(1)
let goodies = string_of_file (Filename.concat lessons_dir "goodies.ml")
let lesson_dirs = list_directory lessons_dir

let langs =
  let langs = ref [] in
  let lang_dir = Filename.concat lessons_dir "lang" in
  List.iter (fun lang ->
    let lang_file = Filename.concat lang_dir
        (Filename.concat lang "messages.ml") in
    try
      if Sys.file_exists lang_file then
        langs := (lang, string_of_file lang_file)  :: !langs;
    with e ->
      Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
        (Printexc.to_string e) lang_file;
      Printf.fprintf stderr "Discarding lang file %s!\n%!" lang_file
  ) (list_directory lang_dir);
  !langs

let int_of_string s =
  try
    int_of_string s
  with _ ->
    Printf.eprintf "int_of_string[%S]: error\n%!" s;
    exit 2

let lesson_of_directory lesson_dir lesson =
  let lesson_num = int_of_string (String.sub lesson 6 (String.length lesson - 6)) in
  let lesson_html = string_of_file (Filename.concat lesson_dir "lesson.html") in
  let lesson_title = get_title lesson_html in
  let lesson_langs =
    let lesson_langs = ref [] in
    List.iter (fun (lang,_) ->
      let file = Filename.concat lesson_dir
          (Printf.sprintf "lesson.html.%s" lang) in
      if Sys.file_exists file then
        let lesson_html = string_of_file file in
        let lesson_title = get_title lesson_html in
        lesson_langs := (lang, { title = lesson_title; html = lesson_html }) :: !lesson_langs
    ) langs;
    !lesson_langs
  in
  let step_dirs = list_directory lesson_dir in
  let steps = ref [] in
  List.iter (fun step ->
    let step_dir = Filename.concat lesson_dir step in
    try
      if string_starts_with step "step" then
        let step_num = int_of_string (String.sub step 4 (String.length step - 4)) in
        let step_html = string_of_file (Filename.concat step_dir "step.html") in
        let step_title = get_title step_html in
        let step_langs =
          let step_langs = ref [] in
          List.iter (fun (lang,_) ->
            let file = Filename.concat step_dir
                (Printf.sprintf "step.html.%s" lang) in
            if Sys.file_exists file then
              let step_html = string_of_file file in
              let step_title = get_title step_html in
              step_langs := (lang, { title = step_title;
                                     html = step_html }) :: !step_langs
          ) langs;
          !step_langs in
        let ml_filename = Filename.concat step_dir "step.ml" in
        let step_ml = string_of_file ml_filename in
        let step_test =
          Printf.sprintf "# 0 %S\n%s" ml_filename step_ml in
        steps := { step_num; step_text = {
                     title = step_title;
                     html = step_html};
                   step_langs;
                   step_test } :: !steps;
    with e ->
      Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
        (Printexc.to_string e) step_dir;
      Printf.fprintf stderr "Discarding step directory !\n%!"
  ) step_dirs;
  let lesson_steps = Array.of_list (List.sort compare !steps) in
  Array.iteri (fun i s ->
    if s.step_num <> i+1 then begin
      Printf.eprintf "Dir %S, step%d is at position %d\n" lesson_dir s.step_num (i+1);
      exit 2;
    end;
  ) lesson_steps;
  {
    lesson_num;
    lesson_text = {
      title = lesson_title;
      html = lesson_html;
    };
    lesson_langs;
    lesson_steps;
  }

type state = NoWhere | InStep | InExam

let lesson_of_file filename lesson_num =
  let ic = open_in filename in
  let lesson_steps = ref [] in
  let step_counter = ref 0 in
  let b = Buffer.create 1000 in
  let lesson_title = ref "NO TITLE" in

  let state = ref NoWhere in
  let step_title = ref "NO TITLE" in
  let step_num = ref 0 in
  let step_html = ref "NO HTML" in
  let commit () =
    begin
      match !state with
      | NoWhere -> ()
      | InStep ->
        step_html := Buffer.contents b
      | InExam ->
        let step = {
          step_num = !step_num;
          step_text = { title = !step_title; html = !step_html };
          step_test = Buffer.contents b;
          step_langs = [];
        } in
        lesson_steps := step :: !lesson_steps
    end;
    Buffer.clear b
  in
  try
    let line_num = ref 0 in
    while true do
      let line = input_line ic in
      incr line_num;
      if string_starts_with line "###" then
        if string_starts_with line "####" then
          ()
        else
          try
            commit ();
            let cmd, arg = cut_at line ':' in
            match cmd with
            | "###lesson" -> lesson_title := arg
            | "###step" ->
              step_title := arg;
              Printf.bprintf b "<h3>%s</h3>\n" arg;
              incr step_counter;
              step_num := !step_counter;
              state := InStep
            | "###exam" | "###test" ->
              Printf.bprintf b "# %d %S\n" !line_num filename;
              state := InExam
            | _ -> raise Not_found
          with _ ->
            Printf.eprintf "File %S, line %d\nSyntax Error\n" filename !line_num;
            exit 2
      else
        Printf.bprintf b "%s\n" line
    done;
    assert false
  with End_of_file ->
    close_in ic;
    commit();
    let lesson_steps = Array.of_list (List.rev !lesson_steps) in
    {
      lesson_num;
      lesson_text = { title = !lesson_title; html = Printf.sprintf "<h3>%s</h3>" !lesson_title };
      lesson_langs = [];
      lesson_steps;
    }

let lesson_of_file filename lesson =
  Printf.eprintf "lesson_of_file[%S]\n%!" lesson;
  let len = String.length lesson in
  let lesson_num = int_of_string (String.sub lesson 6 (len - 10)) in
  let lesson = lesson_of_file filename lesson_num in

  List.iter (fun (lang,_) ->
      let filename = Printf.sprintf "%s.%s" filename lang in
      if Sys.file_exists filename then
        let lesson_lang = lesson_of_file filename lesson_num in
        lesson.lesson_langs <- (lang, lesson_lang.lesson_text) :: lesson.lesson_langs;
        Array.iteri (fun i s ->
          lesson.lesson_steps.(i).step_langs <- (lang, lesson_lang.lesson_steps.(i).step_text) ::
              lesson.lesson_steps.(i).step_langs
        ) lesson_lang.lesson_steps
    ) langs;
  lesson

let lessons =
  let lessons = ref [] in
  List.iter (fun lesson ->
    let lesson_dir = Filename.concat lessons_dir lesson in
    try
      if  string_starts_with lesson "lesson" then
        if Sys.is_directory lesson_dir then
          let lesson = lesson_of_directory lesson_dir lesson in
          lessons := lesson :: !lessons
        else
        if Filename.check_suffix lesson ".hml" then
          let lesson = lesson_of_file lesson_dir lesson in
          lessons := lesson :: !lessons

    with e ->
      Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
        (Printexc.to_string e) lesson_dir;
      Printf.fprintf stderr "Discarding lesson directory !\n%!"
  ) lesson_dirs;
  !lessons

(* We need something to generate OCaml code like this. OCamlify ? Ocaml-data-notation ? *)

let lessons = List.sort compare lessons

let _ =

  Printf.printf "%s\n" goodies;
  Printf.printf "let langs = [\n";
  List.iter (fun (lang, lang_file) ->
    Printf.printf "\t\"%s\", [ %s ];\n" lang lang_file;
  ) langs;
  Printf.printf "]\n";
  Printf.printf "let lessons = [\n";
  List.iter (fun l ->
    Printf.printf "\t(%d, %S, %S, [\n" l.lesson_num
      l.lesson_text.title l.lesson_text.html;
    List.iter (fun (lang, text) ->
      Printf.printf "\t\t(%S, (%S, %S));\n"
        lang text.title text.html;
    ) l.lesson_langs;
    Printf.printf "\t], [\n";
    Array.iter (fun s ->
      Printf.printf "\t\t(%d, %S, %S, [\n" s.step_num
        s.step_text.title s.step_text.html;
      List.iter (fun (lang, text) ->
        Printf.printf "\t\t\t(%S, (%S, %S));\n"
          lang text.title text.html;
      ) s.step_langs;
      Printf.printf "\t\t], (\n";
      Printf.printf "%s\n" s.step_test;
      Printf.printf "\t\t));\n"
    ) l.lesson_steps;
    Printf.printf "\t]);\n";
  ) lessons;
  Printf.printf "];;\n";
  Printf.printf "let register () = \n";
  Printf.printf "  Tutorial.set_langs langs;\n";
  Printf.printf "  Tutorial.set_lessons lessons;\n";
  ()

