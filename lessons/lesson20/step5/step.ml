
let good_output = "val depth : 'a t -> int" in
fun input output ->
  String.sub output 0 (String.length good_output) = good_output


