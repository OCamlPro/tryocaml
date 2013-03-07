(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nat.ml 9547 2010-01-22 12:48:24Z doligez $ *)

open Int_misc

let debug = false

(*
module String = struct
  include String

  let blit src src_pos dst dst_pos len =
    Printf.printf "blit %d[%d] %d[%d] %d\n" src_pos (String.length src)
      dst_pos (String.length dst) len;
    blit src src_pos dst dst_pos len

end
*)

type nat = int array

(* All operations are supposed to happen on signed integers, so we
purposely limit ourselves to 30 bits so support simple integers on
both 32 bits platforms and JS *)

let print_nat msg nat ofs len =
  if debug then begin
    Printf.printf  "%s : [" msg;
    for i = 0 to Array.length nat - 1 do
      Printf.printf "%10d;" nat.(i);
    done;
    Printf.printf "] ofs=%d len=%d\n" ofs len
  end

let length_of_digit = 30
let mask = (1 lsl 30) - 1

let create_nat int = Array.create int 0

let set_to_zero_nat nat pos len =
  for i = pos to pos+len-1 do
    nat.(i) <- 0
  done

let blit_nat dst dst_pos src src_pos src_len =
  Array.blit src src_pos dst dst_pos src_len

let set_digit_nat = Array.set
let nth_digit_nat = Array.get
let set_digit_nat_native nat int natl =
  failwith "set_digit_nat_native: not implementable"

let nth_digit_nat_native nat int =
  Nativeint.of_int nat.(int)

let rec num_digits_nat nat ofs len =
  let len = ref (len-ofs) in
  while !len > 0 && nat.(ofs+ !len-1) = 0 do
    decr len
  done;
  let len = !len in
  if len = 0 then 1 else len

let num_digits_nat nat ofs len =
(*  if debug then Printf.printf "num_digits_nat:\n";
  print_nat "num_digits_nat" nat ofs len; *)
  let num = num_digits_nat nat ofs len in
(*  if debug then Printf.printf "= %d\n" num; *)
  num

let lit0xFFFF0000 = 0xFFFF lsl 16
(* avoid literal size restriction on 32 bits implementations;

   The implementation of num_leading_zero_bits below is in fact still
   correct on [0 .. 2^30-1] when this constant wraps around in the
   negative. The following output comes from a 32 bits version of
   OCaml:

     # Printf.printf "%x" (0xFFFF lsl 16);;
     7fff0000- : unit = ()
     # Printf.printf "%x" (1 lsl 30);;
     40000000- : unit = ()
*)

let num_leading_zero_bits_in_digit nat int =
  let d = ref nat.(int) in
  let n = ref length_of_digit in
  if !d land lit0xFFFF0000 <> 0 then (n := !n - 16; d := !d lsr 16);
  if !d land 0xFF00 <> 0 then (n := !n - 8; d := !d lsr 8);
  if !d land 0xF0 <> 0 then (n := !n - 4; d := !d lsr 4);
  if !d land 0xC <> 0 then (n := !n - 2; d := !d lsr 2);
  if !d land 0x2 <> 0 then (n := !n - 1; d := !d lsr 1);
  !n - !d

let is_digit_int nat int =
  let x = nat.(int) in
  x >= 0 && x land mask = x

let is_digit_zero nat int = nat.(int) = 0
(* let is_digit_normalized nat int = failwith "is_digit_normalized" *)
let is_digit_odd nat int = nat.(int) land 1 = 1
let rec incr_nat nat ofs alen carry =
  if alen = 0 || carry = 0 then carry else
    let n = nat.(ofs) in
    let n = (n + 1) land mask in
    nat.(ofs) <- n;
    if n <> 0 then 0 else
      incr_nat nat (ofs+1) (alen-1) 1

let rec add_nat nat1 ofs1 len1 nat2 ofs2 len2 carry =
  if debug then Printf.printf "add_nat %d %d %d %d %d\n"
    ofs1 len1 ofs2 len2 carry;
  if len1 = 0 then carry else
    let tmp1 = nat1.(ofs1) in
    let tmp2 = if len2 <= 0 then 0 else nat2.(ofs2) in
    let tmp3 = (tmp1 + tmp2) land mask in
    let tmp4 = (tmp3 + carry) land mask in
    nat1.(ofs1) <- tmp4;
    let carry =
      if tmp3 < tmp1 || tmp4 < tmp3 then 1
      else 0
    in
    add_nat nat1 (ofs1+1) (len1-1) nat2 (ofs2+1) (len2-1) carry

let add_nat nat1 ofs1 len1 nat2 ofs2 len2 carry =
  if debug then Printf.printf "add_nat: %d + \n" carry;
  print_nat "nat1" nat1 ofs1 len1;
  print_nat "nat2" nat2 ofs2 len2;
  let carry = add_nat nat1 ofs1 len1 nat2 ofs2 len2 carry in
  print_nat "add_nat (nat1,nat2)= "nat1 ofs1 len1;
  if debug then Printf.printf "carry = %d\n" carry;
  carry

let complement_nat nat ofs len =
  for i = 0 to len - 1 do
    nat.(ofs+i) <- (lnot nat.(ofs + i)) land mask
  done

let decr_nat nat ofs len carry =
  let carry = 1 lxor carry in
  if carry = 0 || len = 0 then carry else
    let rec iter ofs len =
      if len = 0 then 1 else
      let a = nat.(ofs) in
      nat.(ofs) <- (a - 1) land mask;
      let len = len -1 in
      if a > 0 then 0 else
        iter (ofs+1) (len-1)
  in
  1 lxor (iter ofs len)

let sub_nat nat1 ofs1 len1 nat2 ofs2 len2 carry =
  let carry = 1 lxor carry in
  let rec sub_nat nat1 ofs1 len1 nat2 ofs2 len2 carry =
(*    if debug then Printf.printf "sub_nat_in %d\n" carry;
    print_nat "nat1" nat1 ofs1 len1;
    print_nat "nat2" nat2 ofs2 len2; *)
    if len1 = 0 then carry else
      let tmp1 = nat1.(ofs1) in
      let tmp2 = if len2 > 0 then nat2.(ofs2) else 0 in
      let tmp3 = (tmp1 - tmp2) land mask in
      let tmp4 = (tmp3 - carry) land mask in
      nat1.(ofs1) <- tmp4;
      let carry = if tmp1 < tmp2 || tmp3 < carry then 1 else 0 in
      sub_nat nat1 (ofs1+1) (len1-1) nat2 (ofs2+1) (len2-1) carry
  in
  let carry = sub_nat nat1 ofs1 len1 nat2 ofs2 len2 carry in
  let carry = 1 lxor carry in
(*  if debug then Printf.printf "sub_nat finished\n";
  print_nat "sub_nat(nat1, nat2)" nat1 carry 0; *)
  carry

let mult_int_nat nat1 ofs1 len1 nat2 ofs2 len2 d =
  let dL = Int64.of_int d in
  let rec iter carryL ofs1 len1 ofs2 len2 =
    if len1 = 0 then carryL else
      let a = nat1.(ofs1) in
      let b = if len2 > 0 then nat2.(ofs2) else 0 in
      let aL = Int64.of_int a in
      let bL = Int64.of_int b in
      let mL = Int64.mul bL dL in
      let mL = Int64.add mL carryL in
      let mL = Int64.add mL aL in
      let base = (Int64.to_int mL) land mask in
(*      Printf.printf "%d + %d x %d = %d\n" a d b base; *)
      nat1.(ofs1) <- base;
      let carryL = Int64.shift_right_logical mL length_of_digit in
      iter carryL (ofs1+1) (len1-1) (ofs2+1) (len2-1)
  in
  let carryL = iter 0L ofs1 len1 ofs2 len2 in
  Int64.to_int carryL

let mult_digit_nat nat1 ofs1 len1 nat2 ofs2 len2 nat3 ofs3 =
  mult_int_nat  nat1 ofs1 len1 nat2 ofs2 len2 nat3.(ofs3)

let mult_nat nat1 ofs1 len1 nat2 ofs2 len2 nat3 ofs3 len3 =
  let rec iter carry ofs1 len1 ofs3 len3 =
    if len3 = 0 then carry else
    let new_carry = mult_digit_nat
      nat1 ofs1 len1 nat2 ofs2 len2 nat3 ofs3 in
    iter (carry + new_carry)
      (ofs1+1) (len1-1) (ofs3+1) (len3-1)
  in
  iter 0 ofs1 len1 ofs3 len3

(*
let square_nat nat1 ofs1 len1 nat2 ofs2 len2 = failwith "square_nat"
*)

let bng_shift_left_nat nat1 ofs1 len1 nbits =
  if nbits > 0 then
    let shift2 = length_of_digit - nbits in
    let rec iter carry pos =
      if pos < len1 then
        let d = nat1.(ofs1+pos) in
        nat1.(ofs1+pos) <- ((d lsl nbits) lor carry) land mask;
        let carry = (d lsr shift2) land mask in
        iter carry (pos+1)
      else carry
    in
    iter 0 0
  else 0

let shift_left_nat nat1 ofs1 len1 nat2 ofs2 nbits=
  nat2.(ofs2) <- bng_shift_left_nat nat1 ofs1 len1 nbits

let bng_div_digit_nat
    natq ofsq
    nat1 ofs1 len1
    nat2 ofs2 =
  if debug then Printf.printf "div_digit_nat\n";
(*  print_nat  natq ofsq 0;
  print_nat nat1 ofs1 len1;
  print_nat nat2 ofs2 0; *)
  let d = Int64.of_int nat2.(ofs2) in
  let rec iter len1 r =
    if len1 = 0 then r else
      let num = Int64.of_int nat1.(ofs1 + len1 - 1) in
      let num = Int64.add num (Int64.shift_left r length_of_digit) in
      let quo = Int64.div num d in
      let r = Int64.sub num (Int64.mul d quo) in
      natq.(ofsq + len1 - 1) <- Int64.to_int quo;
      iter (len1 - 1) r
  in
  let r = iter (len1-1) (Int64.of_int nat1.(ofs1 + len1 - 1)) in
  if debug then Printf.printf "done\n";
(*  print_nat natq ofsq 0; *)
  Int64.to_int r


let bng_shift_right_nat nat1 ofs1 len1 nbits =
  if nbits > 0 then
    let shift2 = length_of_digit - nbits in
    let rec iter carry pos =
      if pos >= 0 then
        let d = nat1.(ofs1+pos) in
        nat1.(ofs1+pos) <- ((d lsr nbits) lor carry) land mask;
        let carry = (d lsl shift2) land mask in
        iter carry (pos-1)
      else carry
    in
    iter 0 (len1 - 1)
  else
    0

let shift_right_nat nat1 ofs1 len1 nat2 ofs2 nbits =
  nat2.(ofs2) <- bng_shift_right_nat nat1 ofs1 len1 nbits





let compare_digits_nat nat1 ofs1 nat2 ofs2 =
  let d1 = nat1.(ofs1) in
  let d2 = nat2.(ofs2) in
  if d1 > d2 then 1 else
    if d1 < d2 then -1 else 0


let rec compare_nat nat1 ofs1 len1 nat2 ofs2 len2 =

  if len1 > 0 && nat1.(ofs1+len1-1) = 0 then
    compare_nat nat1 ofs1 (len1-1) nat2 ofs2 len2
  else
  if len2 > 0 && nat2.(ofs2+len2-1) = 0 then
    compare_nat nat1 ofs1 len1 nat2 ofs2 (len2-1)
  else
    if len1 > len2 then 1 else
      if len1 < len2 then -1 else
        if len1 = 0 then 0 else
        let v1 = nat1.(ofs1+len1-1) in
        let v2 = nat2.(ofs2+len2-1) in
        if v1 > v2 then 1 else
          if v1 < v2 then -1 else
            compare_nat nat1 ofs1 (len1-1) nat2 ofs2 (len1-1)

let compare_nat nat1 ofs1 len1 nat2 ofs2 len2 =
  if debug then Printf.printf "compare_nat:\n";
  print_nat "n1" nat1 ofs1 len1;
  print_nat "n2" nat2 ofs2 len2;
  let num = compare_nat nat1 ofs1 len1 nat2 ofs2 len2  in
  if debug then Printf.printf "compare_nat = %d\n" num;
  num

let div_digit_nat
    natq ofsq
    natr ofsr
    nat1 ofs1 len1
    nat2 ofs2 =
  natr.(ofsr) <-
    bng_div_digit_nat
    natq ofsq
    nat1 ofs1 len1
    nat2 ofs2;
(*  print_nat natr ofsr 0; *)
  ()

let div_nat n nofs nlen d dofs dlen =
  let shift = num_leading_zero_bits_in_digit d (dofs + dlen - 1) in
  print_nat "n" n nofs nlen;
  print_nat "d" d dofs dlen;
  assert (bng_shift_left_nat n nofs nlen shift = 0);
  assert (bng_shift_left_nat d dofs dlen shift = 0);
  print_nat "n<<" n nofs nlen;
  print_nat "d<<" d dofs dlen;
  begin
    if dlen = 1 then
      n.(nofs) <- bng_div_digit_nat n (nofs+1) n nofs nlen d 0
    else
      let m = create_nat (dlen+1) in
      let ncopy = Array.copy n in

      let topden = d.(dofs + dlen - 1) in
      if debug then Printf.printf "topden = %d\n" topden;
      let topdenplusone = (topden + 1) land mask in
      let rec iter j =
        if j >= dlen then
          let i = j - dlen in
          if debug then Printf.printf "i = %d\n" i;
          let quo = if topdenplusone = 0 then n.(nofs + j)
            else
              let n1L = Int64.of_int n.(nofs + j) in
              let n2L = Int64.of_int n.(nofs + j - 1) in
              let n1L = Int64.shift_left n1L length_of_digit in
              let nL = Int64.logor n1L n2L in
              let qL = Int64.div nL (Int64.of_int topdenplusone) in
              if debug then begin
                Printf.printf "qL = %Ld = (%Ld << %d + %Ld) / %d\n"
                  qL n1L length_of_digit n2L topdenplusone;
                Printf.printf "mL = %Ld <= %Ld\n"
                  (Int64.mul qL (Int64.of_int topden)) nL;
              end;
              Int64.to_int qL
          in
          if debug then begin
            Printf.printf "quo = %d\n" quo;
            print_nat "n<<" n nofs nlen;
          end;
          set_to_zero_nat m 0 (dlen+1);
          let carry1 = mult_int_nat m 0 dlen d dofs dlen quo in
          if debug then begin
            print_nat "d" d 0 dlen;
            print_nat "m" m 0 (dlen+1);
            Printf.printf "carry1 = %d\n" carry1;
          end;
          let carry2 = sub_nat n (nofs + i) dlen m 0 dlen 1 in
          if debug then begin
            Printf.printf "carry2 = %d\n" carry2;
          end;
          n.(nofs + j) <- n.(nofs + j) - carry1 - (1 - carry2);
          if debug then begin
            print_nat "n(new)" n nofs nlen;
          end;
          let quo = ref quo in
          if debug then begin
            Printf.printf "x\n%!";
          end;
          while n.(nofs + j) <> 0 ||
            compare_nat n (nofs + i) dlen d dofs dlen >= 0 do
            if debug then begin
              Printf.printf "y\n%!";
            end;
              incr quo;
              let carry = sub_nat n (nofs + i) dlen d dofs dlen 1 in
              n.(nofs + j) <- n.(nofs + j) - ( 1 - carry)
          done;
          n.(nofs + j) <- !quo;

          let mm = create_nat nlen in
          let _ = mult_nat mm i (nlen-i) d dofs dlen n (nofs+j) (nlen-j)
          in
          print_nat "mm" mm 0 nlen;
          ignore (add_nat mm 0 nlen n nofs j 0);
          print_nat "no" ncopy nofs nlen;
          print_nat "mm" mm 0 nlen;



          iter (j-1)
      in
      let j = nlen - 1 in
      iter j
  end;
  if debug then begin
    print_nat "d>>" d nofs dlen;
    print_nat "n>>" n nofs dlen;
    Printf.printf "shift = %d\n" shift;
  end;
  assert (bng_shift_right_nat n nofs dlen shift = 0);
  assert (bng_shift_right_nat d dofs dlen shift = 0);
  ()


let land_digit_nat nat1 ofs1 nat2 ofs2 =
  nat1.(ofs1) <- nat1.(ofs1) land nat2.(ofs2)
let lor_digit_nat nat1 ofs1 nat2 ofs2=
  nat1.(ofs1) <- nat1.(ofs1) lor nat2.(ofs2)
let lxor_digit_nat nat1 ofs1 nat2 ofs2 =
  nat1.(ofs1) <- nat1.(ofs1) lxor nat2.(ofs2)

let initialize_nat unit= ()

let _ = initialize_nat()

let length_nat (n : nat) = Array.length n

let make_nat len =
  if len < 0 then invalid_arg "make_nat" else
    let res = create_nat len in set_to_zero_nat res 0 len; res

(* Nat temporaries *)
let a_2 = make_nat 2
and a_1 = make_nat 1
and b_2 = make_nat 2

let copy_nat nat off_set length =
 let res = create_nat (length) in
  blit_nat res 0 nat off_set length;
  res

let is_zero_nat n off len =
  compare_nat (make_nat 1) 0 1 n off (num_digits_nat n off len) = 0

let is_nat_int nat off len =
  num_digits_nat nat off len = 1 && is_digit_int nat off

let sys_int_of_nat nat off len =
  if is_nat_int nat off len
  then nth_digit_nat nat off
  else failwith "int_of_nat"

let int_of_nat nat =
  sys_int_of_nat nat 0 (length_nat nat)

let nat_of_int i =
  if i < 0 then invalid_arg "nat_of_int" else
    let res = make_nat 1 in
    if i = 0 then res else begin set_digit_nat res 0 i; res end

let eq_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) = 0
and le_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) <= 0
and lt_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) < 0
and ge_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) >= 0
and gt_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) > 0

(* XL: now implemented in C for better performance.
   The code below doesn't handle carries correctly.
   Fortunately, the carry is never used. *)

let square_nat nat1 off1 len1 nat2 off2 len2 =
  let c = ref 0
  and trash = make_nat 1 in
    (* Double product *)
    for i = 0 to len2 - 2 do
        c := !c + mult_digit_nat
                         nat1
                         (succ (off1 + 2 * i))
                         (2 * (pred (len2 - i)))
                         nat2
                         (succ (off2 + i))
                         (pred (len2 - i))
                         nat2
                         (off2 + i)
    done;
    shift_left_nat nat1 0 len1 trash 0 1;
    (* Square of digit *)
    for i = 0 to len2 - 1 do
        c := !c + mult_digit_nat
                         nat1
                         (off1 + 2 * i)
                         (len1 - 2 * i)
                         nat2
                         (off2 + i)
                         1
                         nat2
                         (off2 + i)
    done;
  !c

let gcd_int_nat i nat off len =
  if i = 0 then 1 else
  if is_nat_int nat off len then begin
    set_digit_nat nat off (gcd_int (nth_digit_nat nat off) i); 0
  end else begin
    let len_copy = succ len in
    let copy = create_nat len_copy
    and quotient = create_nat 1
    and remainder = create_nat 1 in
    blit_nat copy 0 nat off len;
    set_digit_nat copy len 0;
    div_digit_nat quotient 0 remainder 0 copy 0 len_copy (nat_of_int i) 0;
    set_digit_nat nat off (gcd_int (nth_digit_nat remainder 0) i);
    0
  end

let exchange r1 r2 =
  let old1 = !r1 in r1 := !r2; r2 := old1

let gcd_nat nat1 off1 len1 nat2 off2 len2 =
  if is_zero_nat nat1 off1 len1 then begin
    blit_nat nat1 off1 nat2 off2 len2; len2
  end else begin
    let copy1 = ref (create_nat (succ len1))
    and copy2 = ref (create_nat (succ len2)) in
      blit_nat !copy1 0 nat1 off1 len1;
      blit_nat !copy2 0 nat2 off2 len2;
      set_digit_nat !copy1 len1 0;
      set_digit_nat !copy2 len2 0;
      if lt_nat !copy1 0 len1 !copy2 0 len2
         then exchange copy1 copy2;
      let real_len1 =
            ref (num_digits_nat !copy1 0 (length_nat !copy1))
      and real_len2 =
            ref (num_digits_nat !copy2 0 (length_nat !copy2)) in
      while not (is_zero_nat !copy2 0 !real_len2) do
        set_digit_nat !copy1 !real_len1 0;
        div_nat !copy1 0 (succ !real_len1) !copy2 0 !real_len2;
        exchange copy1 copy2;
        real_len1 := !real_len2;
        real_len2 := num_digits_nat !copy2 0 !real_len2
      done;
      blit_nat nat1 off1 !copy1 0 !real_len1;
      !real_len1
  end

(* Racine carrée entière par la méthode de Newton (entière par défaut). *)

(* Théorème: la suite xn+1 = (xn + a/xn) / 2 converge vers la racine *)
(* carrée entière de a par défaut, si on part d'une valeur x0 *)
(* strictement plus grande que la racine de a, sauf quand a est un *)
(* carré - 1, cas auquel la suite alterne entre la racine par défaut *)
(* et par excès. Dans tous les cas, le dernier terme de la partie *)
(* strictement décroissante de la suite est le résultat cherché. *)

let sqrt_nat rad off len =
 let len = num_digits_nat rad off len in
 (* Copie de travail du radicande *)
 let len_parity = len mod 2 in
 let rad_len = len + 1 + len_parity in
 let rad =
   let res = create_nat rad_len in
   blit_nat res 0 rad off len;
   set_digit_nat res len 0;
   set_digit_nat res (rad_len - 1) 0;
   res in
 let cand_len = (len + 1) / 2 in  (* ceiling len / 2 *)
 let cand_rest = rad_len - cand_len in
 (* Racine carrée supposée cand = "|FFFF .... |" *)
 let cand = make_nat cand_len in
 (* Amélioration de la racine de départ:
    on calcule nbb le nombre de bits significatifs du premier digit du candidat
    (la moitié du nombre de bits significatifs dans les deux premiers
     digits du radicande étendu à une longueur paire).
    shift_cand est word_size - nbb *)
 let shift_cand =
   ((num_leading_zero_bits_in_digit rad (len-1)) +
     Sys.word_size * len_parity) / 2 in
 (* Tous les bits du radicande sont à 0, on rend 0. *)
 if shift_cand = Sys.word_size then cand else
 begin
  complement_nat cand 0 cand_len;
  shift_right_nat cand 0 1 a_1 0 shift_cand;
  let next_cand = create_nat rad_len in
  (* Repeat until *)
  let rec loop () =
           (* next_cand := rad *)
   blit_nat next_cand 0 rad 0 rad_len;
           (* next_cand <- next_cand / cand *)
   div_nat next_cand 0 rad_len cand 0 cand_len;
           (* next_cand (poids fort) <- next_cand (poids fort) + cand,
              i.e. next_cand <- cand + rad / cand *)
   ignore (add_nat next_cand cand_len cand_rest cand 0 cand_len 0);
        (* next_cand <- next_cand / 2 *)
   shift_right_nat next_cand cand_len cand_rest a_1 0 1;
   if lt_nat next_cand cand_len cand_rest cand 0 cand_len then
    begin  (* cand <- next_cand *)
     blit_nat cand 0 next_cand cand_len cand_len; loop ()
    end
   else cand in
  loop ()
 end;;

let power_base_max = make_nat 2;;

match length_of_digit with
  | 64 ->
      set_digit_nat power_base_max 0 (Int64.to_int 1000000000000000000L);
      ignore
        (mult_digit_nat power_base_max 0 2
           power_base_max 0 1 (nat_of_int 9) 0)
  | 32 | 31 | 30 -> set_digit_nat power_base_max 0 1_000_000_000
  | _ -> assert false
;;

let pmax =
  match length_of_digit with
  | 64 -> 19
  | 32 | 31 | 30 -> 9
  | _ -> assert false
;;

let max_superscript_10_power_in_int =
  match length_of_digit with
  | 64 -> 18
  | 32| 31 -> 9
  | 30 -> 8
  | _ -> assert false
;;
let max_power_10_power_in_int =
  match length_of_digit with
  | 64 -> nat_of_int (Int64.to_int 1000000000000000000L)
  | 32 | 31 -> nat_of_int 1_000_000_000
  | 30 -> nat_of_int 1_000_000_000
  | _ -> assert false
;;

let raw_string_of_digit nat off =
  if is_nat_int nat off 1
     then begin string_of_int (nth_digit_nat nat off) end
  else begin
       blit_nat b_2 0 nat off 1;
       div_digit_nat a_2 0 a_1 0 b_2 0 2 max_power_10_power_in_int 0;
       let leading_digits = nth_digit_nat a_2 0
       and s1 = string_of_int (nth_digit_nat a_1 0) in
       let len = String.length s1 in
       if leading_digits < 10 then begin
            let result = String.make (max_superscript_10_power_in_int+1) '0' in
            String.set result 0
                         (Char.chr (48 + leading_digits));
            String.blit s1 0
                 result (String.length result - len) len;
            result
       end else begin
            let result = String.make (max_superscript_10_power_in_int+2) '0' in
            String.blit (string_of_int leading_digits) 0 result 0 2;
            String.blit s1 0
                 result (String.length result - len) len;
            result
       end
  end

let raw_string_of_digit nat off =
  if debug then Printf.printf "raw_string_of_digit:\n";
(*  print_nat nat off 0; *)
  let s = raw_string_of_digit nat off in
  if debug then Printf.printf " = [%s]\n" s;
  s

(* XL: suppression de string_of_digit et de sys_string_of_digit.
   La copie est de toute facon faite dans string_of_nat, qui est le
   seul point d entree public dans ce code. *)

(******
let sys_string_of_digit nat off =
    let s = raw_string_of_digit nat off in
    let result = String.create (String.length s) in
    String.blit s 0 result 0 (String.length s);
    s

let string_of_digit nat =
    sys_string_of_digit nat 0

*******)

let digits = "0123456789ABCDEF"

(*
   make_power_base affecte power_base des puissances successives de base a
   partir de la puissance 1-ieme.
   A la fin de la boucle i-1 est la plus grande puissance de la base qui tient
   sur un seul digit et j est la plus grande puissance de la base qui tient
   sur un int.
*)
let make_power_base base power_base =
  let i = ref 0
  and j = ref 0 in
   set_digit_nat power_base 0 base;
   while incr i; is_digit_zero power_base !i do
     ignore
       (mult_digit_nat power_base !i 2
          power_base (pred !i) 1
          power_base 0)
   done;
   while !j < !i-1 && is_digit_int power_base !j do incr j done;
  (!i - 2, !j)

(*
   int_to_string place la representation de l entier int en base base
   dans la chaine s en le rangeant de la fin indiquee par pos vers le
   debut, sur times places et affecte a pos sa nouvelle valeur.
*)
let int_to_string int s pos_ref base times =
  let i = ref int
  and j = ref times in
     while ((!i != 0) || (!j != 0)) && (!pos_ref != -1) do
        String.set s !pos_ref (String.get digits (!i mod base));
        decr pos_ref;
        decr j;
        i := !i / base
     done

(* XL: suppression de adjust_string *)

let power_base_int base i =
  if i = 0 then
    nat_of_int 1
  else if i < 0 then
    invalid_arg "power_base_int"
  else begin
         let power_base = make_nat (succ length_of_digit) in
         let (pmax, pint) = make_power_base base power_base in
         let n = i / (succ pmax)
         and rem = i mod (succ pmax) in
           if n > 0 then begin
               let newn =
                 if i = biggest_int then n else (succ n) in
               let res = make_nat newn
               and res2 = make_nat newn
               and l = num_bits_int n - 2 in
               let p = ref (1 lsl l) in
                 blit_nat res 0 power_base pmax 1;
                 for i = l downto 0 do
                   let len = num_digits_nat res 0 newn in
                   let len2 = min n (2 * len) in
                   let succ_len2 = succ len2 in
                     ignore (square_nat res2 0 len2 res 0 len);
                     if n land !p > 0 then begin
                       set_to_zero_nat res 0 len;
                       ignore
                         (mult_digit_nat res 0 succ_len2
                            res2 0 len2  power_base pmax)
                     end else
                       blit_nat res 0 res2 0 len2;
                     set_to_zero_nat res2 0 len2;
                     p := !p lsr 1
                 done;
               if rem > 0 then begin
                 ignore
                   (mult_digit_nat res2 0 newn
                      res 0 n power_base (pred rem));
                 res2
               end else res
            end else
              copy_nat power_base (pred rem) 1
  end

(* the ith element (i >= 2) of num_digits_max_vector is :
    |                                 |
    | biggest_string_length * log (i) |
    | ------------------------------- | + 1
    |      length_of_digit * log (2)  |
    --                               --
*)

(* XL: ai specialise le code d origine a length_of_digit = 32. *)
(* Puis suppression (inutile?) *)

(******
let num_digits_max_vector =
  [|0; 0; 1024; 1623; 2048; 2378; 2647; 2875; 3072; 3246; 3402;
              3543; 3671; 3789; 3899; 4001; 4096|]

let num_digits_max_vector =
   match length_of_digit with
     16 -> [|0; 0; 2048; 3246; 4096; 4755; 5294; 5749; 6144; 6492; 6803;
              7085; 7342; 7578; 7797; 8001; 8192|]
(* If really exotic machines !!!!
   | 17 -> [|0; 0; 1928; 3055; 3855; 4476; 4983; 5411; 5783; 6110; 6403;
              6668; 6910; 7133; 7339; 7530; 7710|]
   | 18 -> [|0; 0; 1821; 2886; 3641; 4227; 4706; 5111; 5461; 5771; 6047;
              6298; 6526; 6736; 6931; 7112; 7282|]
   | 19 -> [|0; 0; 1725; 2734; 3449; 4005; 4458; 4842; 5174; 5467; 5729;
              5966; 6183; 6382; 6566; 6738; 6898|]
   | 20 -> [|0; 0; 1639; 2597; 3277; 3804; 4235; 4600; 4915; 5194; 5443;
              5668; 5874; 6063; 6238; 6401; 6553|]
   | 21 -> [|0; 0; 1561; 2473; 3121; 3623; 4034; 4381; 4681; 4946; 5183;
              5398; 5594; 5774; 5941; 6096; 6241|]
   | 22 -> [|0; 0; 1490; 2361; 2979; 3459; 3850; 4182; 4468; 4722; 4948;
              5153; 5340; 5512; 5671; 5819; 5958|]
   | 23 -> [|0; 0; 1425; 2258; 2850; 3308; 3683; 4000; 4274; 4516; 4733;
              4929; 5108; 5272; 5424; 5566; 5699|]
   | 24 -> [|0; 0; 1366; 2164; 2731; 3170; 3530; 3833; 4096; 4328; 4536;
              4723; 4895; 5052; 5198; 5334; 5461|]
   | 25 -> [|0; 0; 1311; 2078; 2622; 3044; 3388; 3680; 3932; 4155; 4354;
              4534; 4699; 4850; 4990; 5121; 5243|]
   | 26 -> [|0; 0; 1261; 1998; 2521; 2927; 3258; 3538; 3781; 3995; 4187;
              4360; 4518; 4664; 4798; 4924; 5041|]
   | 27 -> [|0; 0; 1214; 1924; 2428; 2818; 3137; 3407; 3641; 3847; 4032;
              4199; 4351; 4491; 4621; 4742; 4855|]
   | 28 -> [|0; 0; 1171; 1855; 2341; 2718; 3025; 3286; 3511; 3710; 3888;
              4049; 4196; 4331; 4456; 4572; 4681|]
   | 29 -> [|0; 0; 1130; 1791; 2260; 2624; 2921; 3172; 3390; 3582; 3754;
              3909; 4051; 4181; 4302; 4415; 4520|]
   | 30 -> [|0; 0; 1093; 1732; 2185; 2536; 2824; 3067; 3277; 3463; 3629;
              3779; 3916; 4042; 4159; 4267; 4369|]
   | 31 -> [|0; 0; 1057; 1676; 2114; 2455; 2733; 2968; 3171; 3351; 3512;
              3657; 3790; 3912; 4025; 4130; 4228|]
*)
   | 32 -> [|0; 0; 1024; 1623; 2048; 2378; 2647; 2875; 3072; 3246; 3402;
              3543; 3671; 3789; 3899; 4001; 4096|]
   | n -> failwith "num_digits_max_vector"
******)

(* XL: suppression de string_list_of_nat *)

let unadjusted_string_of_nat nat off len_nat =
  let len = num_digits_nat nat off len_nat in
  if len = 1 then
       raw_string_of_digit nat off
  else
       let len_copy = ref (succ len) in
       let copy1 = create_nat !len_copy
       and copy2 = make_nat !len_copy
       and rest_digit = make_nat 2 in
         if len > biggest_int / (succ pmax)
            then failwith "number too long"
            else let len_s = (succ pmax) * len in
                 if debug then Printf.printf "String.length = %d\n" len_s;
                 let s = String.make len_s '0'
                 and pos_ref = ref len_s in
                   len_copy := pred !len_copy;
                   blit_nat copy1 0 nat off len;
                   set_digit_nat copy1 len 0;
                   while not (is_zero_nat copy1 0 !len_copy) do
                     if debug then Printf.printf "pos_ref = %d\n" !pos_ref;
                      div_digit_nat copy2 0
                                     rest_digit 0
                                     copy1 0 (succ !len_copy)
                                     power_base_max 0;
                      let str = raw_string_of_digit rest_digit 0 in
                      String.blit str 0
                                  s (!pos_ref - String.length str)
                                  (String.length str);
                      (* XL: il y avait pmax a la place de String.length str
                         mais ca ne marche pas avec le blit de Caml Light,
                         qui ne verifie pas les debordements *)
                      pos_ref := !pos_ref - pmax;
                      len_copy := num_digits_nat copy2 0 !len_copy;
                      blit_nat copy1 0 copy2 0 !len_copy;
                      set_digit_nat copy1 !len_copy 0
                   done;
                   s

let string_of_nat nat =
  let s = unadjusted_string_of_nat nat 0 (length_nat nat)
  and index = ref 0 in
    begin try
      for i = 0 to String.length s - 2 do
       if String.get s i <> '0' then (index:= i; raise Exit)
      done
    with Exit -> ()
    end;
    String.sub s !index (String.length s - !index)

(* XL: suppression de sys_string_of_nat *)

(* XL: suppression de debug_string_nat *)

let base_digit_of_char c base =
  let n = Char.code c in
    if n >= 48 && n <= 47 + min base 10 then n - 48
    else if n >= 65 && n <= 65 + base - 11 then n - 55
    else failwith "invalid digit"

(*
   La sous-chaine (s, off, len) represente un nat en base base que
   on determine ici
*)
let sys_nat_of_string base s off len =
  let power_base = make_nat (succ length_of_digit) in
  let (pmax, pint) = make_power_base base power_base in
(*  Printf.printf "pmax = %d, pint=%d\n%!" pmax pint;
  print_nat "power_base" power_base 0 0; *)
  let new_len = ref (1 + len / (pmax + 1))
  and current_len = ref 1 in
  let possible_len = ref (min 2 !new_len) in

  let nat1 = make_nat !new_len
  and nat2 = make_nat !new_len

  and digits_read = ref 0
  and bound = off + len - 1
  and int = ref 0 in

  for i = off to bound do
    (*
       on lit pint (au maximum) chiffres, on en fait un int
       et on l integre au nombre
     *)
      let c = String.get s i  in
        begin match c with
          ' ' | '\t' | '\n' | '\r' | '\\' -> ()
        | _ -> int := !int * base + base_digit_of_char c base;
               incr digits_read
        end;
        if (!digits_read = pint || i = bound) && not (!digits_read = 0) then
          begin
           set_digit_nat nat1 0 !int;
           let erase_len = if !new_len = !current_len then !current_len - 1
                           else !current_len in
           for j = 1 to erase_len do
             set_digit_nat nat1 j 0
           done;
           ignore
             (mult_digit_nat nat1 0 !possible_len
                nat2 0 !current_len power_base (pred !digits_read));
           blit_nat nat2 0 nat1 0 !possible_len;
           current_len := num_digits_nat nat1 0 !possible_len;
           possible_len := min !new_len (succ !current_len);
           int := 0;
           digits_read := 0
           end
  done;
  (*
     On recadre le nat
  *)
  let nat = create_nat !current_len in
    blit_nat nat 0 nat1 0 !current_len;
    nat

let nat_of_string s = sys_nat_of_string 10 s 0 (String.length s)

let float_of_nat nat = float_of_string(string_of_nat nat)
