fun input output ->
  find_in " : unit = " output && find_in "Printf.printf" input

