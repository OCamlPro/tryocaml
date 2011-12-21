open Utils

let init_lesson = ref 0
let init_step = ref 0

let select_subject ?(lesson=(!init_lesson)) ?(step=(!init_step)) () = "SUBJECT"

let start_lesson ?(lesson=(!init_lesson)) ?(step=(!init_step)) () =
  let subject = select_subject ~lesson:lesson ~step:step () in
  debug "Starting lesson %d step %d about %s.\n" lesson step subject

let lesson n = 
  init_lesson := n;
  start_lesson ~lesson:(!init_lesson) ~step:(!init_step) ();
  debug "Lesson %d\n Not implemented yet.\n" n

let usage = "usage: " ^ Sys.argv.(0) ^ " [-debug] [-lesson int] [-step int]"

let speclist = [
    ("-debug", Arg.Unit   (fun () -> debug_flag := true), ": set somebool to true");
    ("-lesson", Arg.Int    (fun d -> init_lesson := d),      ": some int parameter");
    ("-step", Arg.Int    (fun d -> init_step := d),      ": some int parameter");
  ]

let main =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  lesson !init_step
  
