
let good_output = "val vect : " in
fun input output ->
  String.sub output 0 (String.length good_output) = good_output


