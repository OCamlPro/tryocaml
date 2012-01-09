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

(* $Id: nat.mli 9547 2010-01-22 12:48:24Z doligez $ *)

(* Module [Nat]: operations on natural numbers *)

type nat

(* Natural numbers (type [nat]) are positive integers of arbitrary size.
   All operations on [nat] are performed in-place. *)

val create_nat: int -> nat
val make_nat: int -> nat
val set_to_zero_nat: nat -> int -> int -> unit
val blit_nat: nat -> int -> nat -> int -> int -> unit
val copy_nat: nat -> int -> int -> nat
val set_digit_nat: nat -> int -> int -> unit
val nth_digit_nat: nat -> int -> int
val set_digit_nat_native: nat -> int -> nativeint -> unit
val nth_digit_nat_native: nat -> int -> nativeint
val length_nat : nat -> int
val num_digits_nat: nat -> int -> int -> int
val num_leading_zero_bits_in_digit: nat -> int -> int
val is_digit_int: nat -> int -> bool
val is_digit_zero: nat -> int -> bool
(* val is_digit_normalized: nat -> int -> bool *)
val is_digit_odd: nat -> int -> bool
val is_zero_nat: nat -> int -> int -> bool
val is_nat_int: nat -> int -> int -> bool
val int_of_nat: nat -> int
val nat_of_int: int -> nat
val incr_nat: nat -> int -> int -> int -> int
val add_nat: nat -> int -> int -> nat -> int -> int -> int -> int
val complement_nat: nat -> int -> int -> unit
val decr_nat: nat -> int -> int -> int -> int
val sub_nat: nat -> int -> int -> nat -> int -> int -> int -> int
val mult_digit_nat: nat -> int -> int -> nat -> int -> int -> nat -> int -> int
val mult_nat: nat -> int -> int -> nat -> int -> int -> nat -> int -> int -> int
val square_nat: nat -> int -> int -> nat -> int -> int -> int
val shift_left_nat: nat -> int -> int -> nat -> int -> int -> unit
val div_digit_nat: nat -> int -> nat -> int -> nat -> int -> int -> nat -> int -> unit
val div_nat: nat -> int -> int -> nat -> int -> int -> unit
val shift_right_nat: nat -> int -> int -> nat -> int -> int -> unit
val compare_digits_nat: nat -> int -> nat -> int -> int
val compare_nat: nat -> int -> int -> nat -> int -> int -> int
val eq_nat : nat -> int -> int -> nat -> int -> int -> bool
val le_nat : nat -> int -> int -> nat -> int -> int -> bool
val lt_nat : nat -> int -> int -> nat -> int -> int -> bool
val ge_nat : nat -> int -> int -> nat -> int -> int -> bool
val gt_nat : nat -> int -> int -> nat -> int -> int -> bool
val land_digit_nat: nat -> int -> nat -> int -> unit
val lor_digit_nat: nat -> int -> nat -> int -> unit
val lxor_digit_nat: nat -> int -> nat -> int -> unit
val gcd_nat : nat -> int -> int -> nat -> int -> int -> int
val sqrt_nat : nat -> int -> int -> nat
val string_of_nat : nat -> string
val nat_of_string : string -> nat
val sys_nat_of_string : int -> string -> int -> int -> nat
val float_of_nat : nat -> float
val make_power_base :  int -> nat -> int * int
val power_base_int : int -> int -> nat
val length_of_digit: int
