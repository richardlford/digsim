(** FloatIO

   float_text_io provides conversion operations between Coq strings
   Coq primitive float. 

   The definitions meant for external use are:

   - nl                                                        A string with a new-line character

   The other definitions are in module FloatIO.

   - Z_to_float (z: Z)                                         Convert Coq integer to float
   - nat_power_of_float (x: float) (n: nat)                    x^n
   - Z_to_string_base10 (min_digits: nat) (num : Z) : string   Convert Z to string with min_digits.
   - pad_to_width (width: nat) (s: string)                     Pad string with leading blanks up to width
   - float_to_string (width fdigs: nat) (x: float)             Convert float to string like Ew.(fdigs-1)
   - float_list_to_string (width fdigs: nat) (add_new_line: bool) (al : list float)
                                                               Convert list of floats to string using
                                                               float_to_string width fdigs, and if
                                                               add_new_line is true, append a new line
                                                               at the end.
 *)
(* Check Decimal.decimal. *)
Require Import Coq.Floats.Floats. (* Coq primitive floats *)

Require Import Coq.Lists.List.
Require Import BinNums.
Require Import Coq.QArith.QArith_base.
Require Import Strings.String.
Require Import Strings.Ascii.
Require Import Coq.ZArith.Znat.
Require Import Recdef.
Require Import Task.monad.

(** Pretty printing
 *)
Open Scope Z.
Open Scope bool_scope.
Open Scope string_scope.
Definition nl := String "010"%char EmptyString.
Close Scope char_scope.

(* Compute SF2Prim (Prim2SF 1.5). *)

Module FloatIO.
Check float.
  (* Convert Coq integer to float *)
  Definition Z_to_float (z: Z) :=
    match z with
    | Z0 => 0.0%float
    | Zpos x => SF2Prim (S754_finite false x 0)
    | Zneg x => SF2Prim (S754_finite true x 0)
    end.

  Fixpoint nat_power_of_float (x: float) (n: nat) : float :=
    match n with
    | O => 1.0
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
Fixpoint Z_to_string_base10_aux (min_digits fuel: nat) (num : Z) : string :=
  if (num <? 0) then
    "Require non-negative num."
  else
    match fuel with
    | O => "out of fuel"
    | S x => match num with
            | 0 => repeat_string min_digits "0"
            | _ => (Z_to_string_base10_aux (pred min_digits) x (num / 10)) ++ digits (num mod 10)
            end
    end.

Definition Z_to_string_base10 (min_digits: nat) (num : Z) : string :=
  if num <? 0 then
    "-" ++ Z_to_string_base10_aux min_digits 100 (-num)
  else
    if (num =? 0)%Z then
      repeat_string min_digits "0"
    else
      Z_to_string_base10_aux min_digits 100 num.

(* Like Z_to_string_base10 but always print sign *)
Definition Z_to_signed_string_base10 (min_digits: nat) (num : Z) : string :=
  if num >=? 0 then
    "+" ++ Z_to_string_base10 min_digits num
  else Z_to_string_base10 min_digits num.

(* Helper function when computing the digits of an integer base 16. *)
Fixpoint Z_to_string_base16_aux (min_digits fuel: nat) (num : Z) : string :=
  if (num <? 0) then
    "Require non-negative num."
  else
    match fuel with
    | O => "out of fuel"
    | S x => match num with
            | 0 => repeat_string min_digits "0"
            | _ => (Z_to_string_base16_aux (pred min_digits) x (num / 16)) ++ digits (num mod 16)
            end
    end.

Definition Z_to_string_base16 (min_digits: nat) (num : Z) : string :=
  if num <? 0 then
    "-" ++ Z_to_string_base16_aux min_digits 100 (-num)
  else
    if (num =? 0)%Z then
      repeat_string min_digits "0"
    else
      Z_to_string_base16_aux min_digits 100 num.

Definition f10 : float := 10.0.

Fixpoint pos_power_of_float_aux  (exp: positive) (base accum: float) : float :=
  match exp with
  | xI x => pos_power_of_float_aux x (base * base) (accum * base)
  | xO x => pos_power_of_float_aux x (base * base) accum
  | xH => accum * base
  end.

Definition pos_power_of_float (base: float) (exp: positive) :=
  pos_power_of_float_aux exp base 1.0.

(* Compute pos_power_of_float 10.0 5. *)

Definition Z_power_of_float (base: float) (exp: Z) : float :=
  match exp with
  | Z0 => 1.0
  | Zpos x => pos_power_of_float base x
  | Zneg x => 1.0 / pos_power_of_float base x
  end.

(*
Compute Z_power_of_float 10.0 0.
Compute Z_power_of_float 10.0 1.
Compute Z_power_of_float 10.0 2.
Compute Z_power_of_float 10.0 3.
Compute Z_power_of_float 10.0 (-1).
Compute Z_power_of_float 10.0 (-2).
Compute Z_power_of_float 10.0 (-3).
*)

Definition pow10 (e: Z) := Z_power_of_float 10.0 e.

Definition cond_Zopp (sign: bool) (z: Z) :=
  if sign then -z else z.

(*
Compute cond_Zopp true 3.
Compute cond_Zopp false 3.
*)

Definition ZofB (f: float): option Z :=
  let sf := Prim2SF f in
  match sf with
  | S754_zero s => Some 0
  | S754_infinity s => None
  | S754_nan => None
  | S754_finite s m (Zpos e) => Some (cond_Zopp s (Zpos m) * Z.pow_pos 2 e)%Z
  | S754_finite s m 0 => Some (cond_Zopp s (Zpos m))
  | S754_finite s m (Zneg e) => Some (cond_Zopp s (Zpos m / Z.pow_pos 2 e))%Z
  end.

(* Compute ZofB 1234.5. *)

(* Convert float to Z scaled by 10**fdigs. *)
Definition scaled_float_to_Z (x : float) (fdigs: Z) :=
  match ZofB (x * (pow10 fdigs)) with
  | Some ii => ii
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
  let sf := Prim2SF x in
  match sf with
  | S754_finite false m e => 
    let digs_after_dec := pred fdigs in
    let scale := scale_exp (Z.of_nat fdigs) e in
    let scaled := scaled_float_to_Z x scale in
    let b10 := Z_to_string_base10 1 scaled in
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
          Z_to_string_base10 fdigs scaled'
        else
          b10
            in
    let d10 := insert_decimal b10' digs_after_dec in
    let true_exp := (Z.of_nat fdigs) - scale' - 1 in
    let exp_string := Z_to_signed_string_base10 2 true_exp in
    d10 ++ "e" ++ exp_string
  | _ => ""
  end.

(* Compute float_to_string_unsigned 12.0625%float 7. *)

Definition zero_to_string_unpadded (fdigs: nat) := 
  let digs_after_dec := pred fdigs in
  let frac := repeat_string digs_after_dec "0" in
  "0." ++ frac ++ "e+00".

Definition float_to_string_unpadded (x: float) (fdigs: nat) :=
  let sf := Prim2SF x in
  match sf with
  | S754_zero false => "" ++ zero_to_string_unpadded fdigs
  | S754_zero true => "-" ++ zero_to_string_unpadded fdigs
  | S754_infinity false => "inf"
  | S754_infinity true => "-inf"
  | S754_nan => "nan"
  | S754_finite false m e => float_to_string_unsigned x fdigs
  | S754_finite true m e => "-" ++ float_to_string_unsigned (abs x) fdigs
  end.

(*
Compute float_to_string_unpadded 12.0625%float 7.
Compute float_to_string_unpadded (-12.0625%float) 7.
Compute float_to_string_unpadded (0.0%float) 7.
Compute float_to_string_unpadded (-0.0%float) 7.
Compute float_to_string_unpadded infinity 7.
Compute float_to_string_unpadded (-infinity) 7.
Compute float_to_string_unpadded nan 7.
 *)

Definition pad_to_width (width: nat) (s: string) :=
  let ls := length s in
  let pads := repeat_string (width - ls) " " in
  pads ++ s.

(* Float to string in Ew.d format, with width w and digits d. *)
Definition float_to_string (width fdigs: nat) (x: float)  :=
  pad_to_width width (float_to_string_unpadded x fdigs).

(* Compute float_to_string 14 7 12.0625%float. *)
(* Compute float_to_string 14 7 (-12.0625%float). *)
(* Compute float_to_string 14 7 (0.0%float). *)
(* Compute float_to_string 14 7 (-0.0%float). *)
(* Compute float_to_string 14 7 infinity. *)
(* Compute float_to_string 14 7 (-infinity). *)
(* Compute float_to_string 14 7 nan. *)

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
Definition ZofFloat (f: float) :=
  match Details.ZofB f with
  | Some z => z
  | None => 0%Z
  end.

Definition round (f: float) : float :=
  let z := ZofFloat (f + 0.5) in
  Z_to_float z.

End FloatIO.
