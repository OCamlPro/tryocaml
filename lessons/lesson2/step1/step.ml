let good_output_start = "val y : int = 43" in
fun input output ->
  String.sub output 0 (String.length good_output_start) = good_output_start
