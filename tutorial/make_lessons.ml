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

let _ =
  let lessons_dir = Sys.argv.(1) in
  let goodies = string_of_file (Filename.concat lessons_dir "goodies.ml") in
  let lesson_dirs = list_directory lessons_dir in
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
  let langs = !langs in
  let lessons = ref [] in
  List.iter (fun lesson ->
    let lesson_dir = Filename.concat lessons_dir lesson in
    try
      if string_starts_with lesson "lesson" then
        let lesson_num = int_of_string (String.sub lesson 6 (String.length lesson - 6)) in
        let lesson_html = string_of_file (Filename.concat lesson_dir "lesson.html") in
        let lesson_title = get_title lesson_html in
        let lesson_langs = ref [] in
        List.iter (fun (lang,_) ->
          let file = Filename.concat lesson_dir
            (Printf.sprintf "lesson.html.%s" lang) in
          if Sys.file_exists file then
            let lesson_html = string_of_file file in
            let lesson_title = get_title lesson_html in
            lesson_langs := (lang, lesson_title, lesson_html) :: !lesson_langs
        ) langs;
        let step_dirs = list_directory lesson_dir in
        let steps = ref [] in
        List.iter (fun step ->
          let step_dir = Filename.concat lesson_dir step in
          try
            if string_starts_with step "step" then
              let step_num = int_of_string (String.sub step 4 (String.length step - 4)) in
              let step_html = string_of_file (Filename.concat step_dir "step.html") in
              let step_title = get_title step_html in
              let step_langs = ref [] in
              List.iter (fun (lang,_) ->
                let file = Filename.concat step_dir
                  (Printf.sprintf "step.html.%s" lang) in
                if Sys.file_exists file then
                  let step_html = string_of_file file in
                  let step_title = get_title step_html in
                  step_langs := (lang, step_title, step_html) :: !step_langs
              ) langs;
              let ml_filename = Filename.concat step_dir "step.ml" in
              let step_ml = string_of_file ml_filename in
              steps := (step_num, step_title, step_html, !step_langs, ml_filename, step_ml) :: !steps;
          with e ->
            Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
              (Printexc.to_string e) step_dir;
            Printf.fprintf stderr "Discarding step directory !\n%!"
        ) step_dirs;
        let steps = List.sort compare !steps in
        lessons := (lesson_num, lesson_title, lesson_html, !lesson_langs, steps) :: !lessons
    with e ->
      Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
        (Printexc.to_string e) lesson_dir;
      Printf.fprintf stderr "Discarding lesson directory !\n%!"
  ) lesson_dirs;

(* We need something to generate OCaml code like this. OCamlify ? Ocaml-data-notation ? *)

  let lessons = List.sort compare !lessons in

  Printf.printf "%s\n" goodies;
  Printf.printf "let langs = [\n";
  List.iter (fun (lang, lang_file) ->
    Printf.printf "\t\"%s\", [ %s ];\n" lang lang_file;
  ) langs;
  Printf.printf "]\n";
  Printf.printf "let lessons = [\n";
  List.iter (fun (lesson_num, lesson_title, lesson_html, lesson_langs, steps) ->
    Printf.printf "\t(%d, \"%s\", \"%s\", [\n" lesson_num
      (String.escaped lesson_title) (String.escaped lesson_html);
    List.iter (fun (lang, lesson_title, lesson_html) ->
      Printf.printf "\t\t(\"%s\", (\"%s\", \"%s\"));\n"
        lang (String.escaped lesson_title) (String.escaped lesson_html);
    ) lesson_langs;
    Printf.printf "\t], [\n";
    List.iter (fun (step_num, step_title, step_html, step_langs, ml_filename, step_ml) ->
      Printf.printf "\t\t(%d, \"%s\", \"%s\", [\n" step_num
        (String.escaped step_title) (String.escaped step_html);
      List.iter (fun (lang, step_title, step_html) ->
        Printf.printf "\t\t\t(\"%s\", (\"%s\", \"%s\"));\n"
          lang (String.escaped step_title) (String.escaped step_html);
      ) step_langs;
      Printf.printf "\t\t], (\n";
      Printf.printf "# 0 \"%s\"\n" ml_filename;
      Printf.printf "%s\n" step_ml;
      Printf.printf "\t\t));\n"
    ) steps;
    Printf.printf "\t]);\n";
  ) lessons;
  Printf.printf "];;\n";
  ()

