
let ppf =  Format.std_formatter

(*
open Big

(*
let _ =
  let t = ref (int_of_int 1_000) in
  for i = 0 to 1000 do
    let t0 = !t in
    let t1 = !t + !t + !t + !t in
    t := t1;
    Format.fprintf ppf "------------------------------------------------";
    print ppf !t;
    let t2 = t1 - t0 - t0 - t0 in
    Format.fprintf ppf "----------------";
    print ppf  t2;
  done
*)

let _ =
  let t = ref (int_of_int 1_000) in
  for i = 0 to 100 do
    t := !t * !t;
    Format.fprintf ppf "------------------------------------------------";
    print ppf !t;

  done
*)
let _ =
  let s = "20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" in
  for i = 1 to String.length s do
    let s = String.sub s 0 i in
    Printf.printf "s = %s [%d]\n%!" s i;
    assert (s = Big_int.string_of_big_int (Big_int.big_int_of_string s))
  done


