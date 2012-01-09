
let ppf =  Format.std_formatter

open Big

let _ =
  let t = ref (big 1_000) in
  for i = 0 to 100 do
    t := !t + !t;
    Format.fprintf ppf "------------------------------------------------";
    print ppf !t;
  done

