let big_int_of_string = Big_int.big_int_of_string
let string_to_big_int = Big_int.big_int_of_string

let print_big_int ppf b =
  Format.fprintf ppf "@[%sI@]@." (Big_int.string_of_big_int b)

let print_num ppf b =
  Format.fprintf ppf "@[Num %s@]@." (Num.string_of_num b)

module I = struct

  open Big_int

  let (+) = add_big_int
  let (-) = sub_big_int
  let ( * ) = mult_big_int
  let (/) = div_big_int

end

module R = struct

  open Num

  let (+) = add_num
  let (-) = sub_num
  let ( * ) = mult_num
  let (/) = div_num

  let int_of_int = Num.num_of_int
  external int_to_int : int -> int = "%identity"
  let big_int_of_string = Num.num_of_string
  let string_to_big_int = Big_int.big_int_of_string

end

let init () =
  Num.init ();
  Big_int.init ()

