open Unix

(** {1 I/O operations} *)
(** Set the flag to [true] if you want to activate the debug mode. *)
let debug_flag = ref false

(** If [debug_flag] is set to [true], then print in stderr. *)
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

