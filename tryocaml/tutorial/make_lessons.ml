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

let _ =
  let lessons_dir = "../../lessons" in
  let lesson_dirs = list_directory lessons_dir in
  let lessons = ref [] in
  List.iter (fun lesson ->
    let lesson_dir = Filename.concat lessons_dir lesson in
    try
      assert ( String.sub lesson 0 6 = "lesson" );
      let lesson_num = int_of_string (String.sub lesson 6 (String.length lesson - 6)) in
      let step_dirs = list_directory lesson_dir in
      let steps = ref [] in
      List.iter (fun step ->
        let step_dir = Filename.concat lesson_dir step in
        try
          assert (String.sub step 0 4 = "step" );
          let step_num = int_of_string (String.sub step 4 (String.length step - 4)) in
          let step_txt = string_of_file (Filename.concat step_dir "step.html") in
          let ml_filename = Filename.concat step_dir "step.ml" in
          let step_ml = string_of_file ml_filename in
          steps := (step_num, step_txt, ml_filename, step_ml) :: !steps;
        with e ->
          Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
            (Printexc.to_string e) step_dir;
          Printf.fprintf stderr "Discarding step directory !\n%!"
      ) step_dirs;
      let steps = List.sort compare !steps in
      lessons := (lesson_num, steps) :: !lessons
    with e ->
      Printf.fprintf stderr "Warning: exception %s while inspecting %s\n%!"
        (Printexc.to_string e) lesson_dir;
      Printf.fprintf stderr "Discarding lesson directory !\n%!"
  ) lesson_dirs;

  let lessons = List.sort compare !lessons in

  Printf.printf "let lessons = [\n";
  List.iter (fun (lesson_num, steps) ->
    Printf.printf "\t(%d, [\n" lesson_num;
    List.iter (fun (step_num, step_txt, ml_filename, step_ml) ->
      Printf.printf "\t\t(%d, \"%s\", (\n" step_num (String.escaped step_txt);
      Printf.printf "# 0 \"%s\"\n" ml_filename;
      Printf.printf "%s\n" step_ml;
      Printf.printf "\t\t));"
    ) steps;
    Printf.printf "\t]);";
  ) lessons;
  Printf.printf "];;\n";
  ()

