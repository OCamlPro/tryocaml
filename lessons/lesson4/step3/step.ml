fun input output ->
  find_in "  | (_, x) when x=42 -> false" input &&
  find_in "val fit : string -> int -> bool =" output
