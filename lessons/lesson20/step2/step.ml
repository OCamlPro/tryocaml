
let good_input = ".(" in
fun input output ->
  try
    for i = 0 to String.length input do
      if String.sub input i (String.length good_input) = good_input then
        raise Exit
    done;
    false
  with Exit -> true


