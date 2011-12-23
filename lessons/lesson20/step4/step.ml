
let good_output = "val abs : vect -> " in
fun input output ->
  String.sub output 0 (String.length good_output) = good_output


