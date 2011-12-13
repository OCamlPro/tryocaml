open Unix

(* Set the flag to true if you want to activate the debug mode *)
let debug_flag = ref true

let debug fmt = 
  if !debug_flag then Printf.eprintf fmt 
  else Printf.ifprintf Pervasives.stderr fmt

let open_in_file input_filename =
  try
    open_in input_filename
  with Sys_error _ ->
    failwith (Printf.sprintf "Cannot open the file %s" input_filename)

let open_out_file output_filename =
  try
    open_out output_filename
  with Sys_error _ ->
    failwith (Printf.sprintf "Cannot open the file %s" output_filename)
 
(* let walk_directory_tree dir pattern = *)
(*   let select str = Str.string_match (Str.regexp pattern) str 0 in *)
(*   let rec walk acc = function *)
(*   | [] -> (acc) *)
(*   | dir::tail -> *)
(*       let contents = Array.to_list (Sys.readdir dir) in *)
(*       let contents = List.rev_map (Filename.concat dir) contents in *)
(*       let dirs, files = *)
(*         List.fold_left (fun (dirs,files) f -> *)
(*              match (Unix.stat f).Unix.st_kind with *)
(*              | S_REG -> (dirs, f::files)  (\* Regular file *\) *)
(*              | S_DIR -> (f::dirs, files)  (\* Directory *\) *)
(*              | _ -> (dirs, files) *)
(*           ) ([],[]) contents *)
(*       in *)
(*       let matched = List.filter (select) files in *)
(*       walk (matched @ acc) (dirs @ tail) *)
(*   in *)
(*   walk [] [dir] *)

 
(* let () = *)
(*   let results = walk_directory_tree "/usr/local/lib/ocaml"  ".*\\.cma" in *)
(*   List.iter print_endline results; *)

