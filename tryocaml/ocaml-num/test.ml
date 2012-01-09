
let ppf =  Format.std_formatter

open Big

let _ =
  let t = ref (int_of_int 1_000) in
  for i = 0 to 1000 do
    t := !t + !t + !t + !t;
    Format.fprintf ppf "------------------------------------------------";
    print ppf !t;
  done

