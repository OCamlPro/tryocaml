
open Big_int

let print ppf b =
  Format.fprintf ppf "@[Big %s@]@." (string_of_big_int b)

let (+) = add_big_int
let (-) = sub_big_int
let ( * ) = mult_big_int
let (/) = div_big_int

let str = big_int_of_string
let int_of_int = big_int_of_int

let init () = ()
