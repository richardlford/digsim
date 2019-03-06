(** FloatIO

   Coqsim_io provides conversion operations between Coq strings
   and the Coq/Compcert data types used in simulation.

   The definitions meant for external use are:

   - nl                                                        A string with a new-line character

   The other definitions are in module FloatIO.

   - Z_to_float (z: Z)                                         Convert Coq integer to float
   - fraction_to_float (m d: Z)                                Convert fraction m*(10^(-d)) to float.
   - nat_power_of_float (x: float) (n: nat)                    x^n
   - Z_to_string_base10 (num : Z) (min_digits: nat) : string   Convert Z to string with min_digits.
   - pad_to_width (width: nat) (s: string)                     Pad string with leading blanks up to width
   - float_to_string (width fdigs: nat) (x: float)             Convert float to string like Ew.(fdigs-1)
   - float_list_to_string (width fdigs: nat) (add_new_line: bool) (al : list float)
                                                               Convert list of floats to string using
                                                               float_to_string width fdigs, and if
                                                               add_new_line is true, append a new line
                                                               at the end.
 *)
From compcert Require Import Floats.
Import Float.
From compcert Require Import Fappli_IEEE.
From compcert Require Import Integers.
Require Import Coq.Lists.List.
Require Import BinNums.
Require Import Coq.QArith.QArith_base.
Require Import Strings.String.
Require Import Strings.Ascii.
Require Import Coq.ZArith.Znat.
Require Import Recdef.

(** Pretty printing
 *)
Open Scope Z.
Open Scope bool_scope.
Open Scope string_scope.
Definition nl := String "010"%char EmptyString.
Close Scope char_scope.

(* Version 8.8 has concat but earlier versions don't so include it
   for compatibility.
 *)

(** *** Concatenating lists of strings *)

(** [concat sep sl] concatenates the list of strings [sl], inserting
    the separator string [sep] between each. *)

Fixpoint concat (sep : string) (ls : list string) :=
  match ls with
  | nil => EmptyString
  | cons x nil => x
  | cons x xs => x ++ sep ++ concat sep xs
  end.



Module FloatIO.
  (* Convert Coq integer to float *)
  Definition Z_to_float (z: Z) :=
    match z with
    | Z0 => Float.zero
    | Zpos x => from_parsed 10 x 0
    | Zneg x => neg (from_parsed 10 x 0)
    end.

  (* Convert a decimal fraction to float.
     First parameter, m, is an integer with all the
     significant digits. The second
     parameter, d, is the number of digits that
     should follow the decimal point.
     The resulting values is

     m * (10^(-d)).
     
   *)
  Definition fraction_to_float (m d: Z) :=
    match m with
    | Z0 => Float.zero
    | Zpos x => from_parsed 10 x (-d)
    | Zneg x => neg (from_parsed 10 x (-d))
    end.

  Definition floating_one := Z_to_float 1.

  Fixpoint nat_power_of_float (x: float) (n: nat) : float :=
    match n with
    | O => floating_one
    | S p => (mul x (nat_power_of_float x p))
    end.
  
Module Details.
  
Definition precision_of_float := 53%Z.
Definition log10of2scaled3 := 301.

(* Compute approximate scaling exponent of 10, f, needed to
   get a number with given binary exponent, e, in the range
   10^(d-1) <= value < 10^d

   so we can extract d significant digits.
 *)
Definition scale_exp (d e: Z) :=
  d - 2 - (e + precision_of_float - 1)*log10of2scaled3/1000.

Definition digits (n : Z) : string :=
  match n with
  | 0  => "0" | 6  => "6" | 12 => "c" | 18 => "i" | 24 => "o" | 30 => "u"
  | 1  => "1" | 7  => "7" | 13 => "d" | 19 => "j" | 25 => "p" | 31 => "v"
  | 2  => "2" | 8  => "8" | 14 => "e" | 20 => "k" | 26 => "q" | 32 => "w"
  | 3  => "3" | 9  => "9" | 15 => "f" | 21 => "l" | 27 => "r" | 33 => "x"
  | 4  => "4" | 10 => "a" | 16 => "g" | 22 => "m" | 28 => "s" | 34 => "y"
  | 5  => "5" | 11 => "b" | 17 => "h" | 23 => "n" | 29 => "t" | 35 => "z"
  | _  => "! Error. Max radix is 35 !"
  end. 

(* Repeat the given string n times. *)
Fixpoint repeat_string (n: nat) (c: string) : string :=
  match n with
  | O => ""
  | S x => c ++ repeat_string x c
  end.

(* Helper function when computing the digits of an integer base 10. *)
Fixpoint Z_to_string_base10_aux (num : Z) (min_digits fuel: nat) : string :=
  if (num <? 0) then
    "Require non-negative num."
  else
    match fuel with
    | O => "out of fuel"
    | S x => match num with
            | 0 => repeat_string min_digits "0"
            | _ => (Z_to_string_base10_aux (num / 10) (pred min_digits) x) ++ digits (num mod 10)
            end
    end.

Definition Z_to_string_base10 (num : Z) (min_digits: nat) : string :=
  if num <? 0 then
    "-" ++ Z_to_string_base10_aux (-num) min_digits 100
  else
    if (num =? 0)%Z then
      repeat_string min_digits "0"
    else
      Z_to_string_base10_aux num min_digits 100.

(* Convert float to Z scaled by 10**fdigs. *)
Definition scaled_float_to_Z (x : float) (fdigs: Z) :=
  match to_long (mul x (from_parsed 10 1 fdigs)) with
  | Some ii => Int64.signed ii
  | None    => 0%Z
  end.

(* Insert decimal point so there are fdigs digits after
   the decimal point.
 *)
Definition insert_decimal(s: string) (fdigs: nat) :=
  let len := length s in
  let front := (len - fdigs)%nat in
  (substring 0 front s) ++ "." ++ (substring front fdigs s).

(* Convert positive finite float to string with given
   number of digits of precision. Other floats return
   empty string as they are covered by the 
   floatToStringUnpadded function below.
*)
Definition float_to_string_unsigned (x: float) (fdigs: nat) :=
  match x with
  | B754_finite false m e _ =>
    let digs_after_dec := pred fdigs in
    let scale := scale_exp (Z.of_nat fdigs) e in
    let scaled := scaled_float_to_Z x scale in
    let b10 := Z_to_string_base10 scaled 1 in
    let lb10 := length b10 in
    let scale' :=
        if (lb10 <? fdigs)%nat then
          scale + (Z.of_nat (fdigs - lb10))
        else
          scale
            in
    let b10' :=
        if (lb10 <? fdigs)%nat then
          let scaled' := scaled_float_to_Z x scale' in
          Z_to_string_base10 scaled' fdigs
        else
          b10
            in
    let d10 := insert_decimal b10' digs_after_dec in
    let true_exp := (Z.of_nat fdigs) - scale' - 1 in
    let exp_string := Z_to_string_base10 true_exp 1 in
    d10 ++ "e" ++ exp_string
  | _ => ""
  end.

Definition float_to_string_unpadded (x: float) (fdigs: nat) :=
  match x with
  | B754_zero false => "0.0"
  | B754_zero true => "-0.0"
  | B754_infinity false => "inf"
  | B754_infinity true => "-inf"
  | B754_nan false _ => "nan"
  | B754_nan true _ => "-nan"
  | B754_finite false m e _ => float_to_string_unsigned x fdigs
  | B754_finite true m e _ => "-" ++ float_to_string_unsigned (abs x) fdigs
  end.

Definition pad_to_width (width: nat) (s: string) :=
  let ls := length s in
  let pads := repeat_string (width - ls) " " in
  pads ++ s.

(* Float to string in Ew.d format, with width w and digits d. *)
Definition float_to_string (width fdigs: nat) (x: float)  :=
  pad_to_width width (float_to_string_unpadded x fdigs).

Fixpoint float_list_to_string (width fdigs: nat) (add_new_line: bool) (al : list float) : string :=
  match al with
  | nil => if add_new_line then nl else ""
  | hd :: tl => (float_to_string width fdigs hd) ++ (float_list_to_string width fdigs add_new_line tl)
  end.

End Details.

(* Export items from the details module that the user will want. *)
Definition Z_to_string_base10 := Details.Z_to_string_base10.
Definition pad_to_width := Details.pad_to_width.
Definition float_to_string := Details.float_to_string.
Definition float_list_to_string := Details.float_list_to_string.

End FloatIO.
