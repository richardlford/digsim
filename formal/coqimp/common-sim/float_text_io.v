(** FloatIO

   Coqsim_io provides conversion operations between Coq strings
   and the Coq/Compcert data types used in simulation.

   The definitions meant for external use are:

   - nl                                                        A string with a new-line character

   The other definitions are in module FloatIO.

   - Z_to_float (z: Z)                                         Convert Coq integer to float
   - fraction_to_float (m d: Z)                                Convert fraction m*(10^(-d)) to float.
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
From compcert Require Import Floats.
Import Float.
From compcert Require Import IEEE754.Bits.
From compcert Require Import IEEE754_extra.
From compcert Require Import IEEE754.Binary.

From compcert Require Import Integers.
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

Record FloatParts : Set :=
  {
    mantissaSign: bool;
    intPart: string;
    fracPart: string;
    expPart: string
  }.

Fixpoint strToUintHelper (s: string) : option Decimal.uint :=
  match s with
  | EmptyString => Some Decimal.Nil
  | String x x0 =>
    lower <- strToUintHelper x0;
      match x with
      | "0"%char => Some (Decimal.D0 lower)
      | "1"%char => Some (Decimal.D1 lower)
      | "2"%char => Some (Decimal.D2 lower)
      | "3"%char => Some (Decimal.D3 lower)
      | "4"%char => Some (Decimal.D4 lower)
      | "5"%char => Some (Decimal.D5 lower)
      | "6"%char => Some (Decimal.D6 lower)
      | "7"%char => Some (Decimal.D7 lower)
      | "8"%char => Some (Decimal.D8 lower)
      | "9"%char => Some (Decimal.D9 lower)
      | _ => None
      end
    end.


Definition strToUint (s: string) : option Decimal.uint :=
  x <- strToUintHelper s;
  Some (Decimal.unorm x).

(*
Compute strToUint "001234".
Compute strToUint "".
Compute strToUint "0".
Compute strToUint "-3".
*)

Definition strToIntHelper (s: string) : option Decimal.int :=
  match s with
  | EmptyString => Some (Decimal.Pos Decimal.zero)
  | String x x0 =>
    let (sign, digits) := 
        match x with
        | "+"%char => (false, x0)
        | "-"%char => (true, x0)
        | _ => (false, s)
        end in
    uintVal <- strToUint digits;
      Some (if sign then Decimal.Neg uintVal else Decimal.Pos uintVal)
  end.

Definition strToInt (s: string) : option Decimal.int :=
  x <- strToIntHelper s;
  Some (Decimal.norm x).

(*
Compute strToInt "001234".
Compute strToInt "-001234".
Compute strToInt "+001234".
Compute strToInt "".
Compute strToInt "+".
Compute strToInt "-".
Compute strToInt "0".
Compute strToInt "-0".
Compute strToInt "-3".
*)

Definition strToZ (s: string) : option Z :=
  x <- strToInt s;
    Some (Z.of_int x).

(*
Compute strToZ "001234".
Compute strToZ "-001234".
Compute strToZ "+001234".
Compute strToZ "".
Compute strToZ "+".
Compute strToZ "-".
Compute strToZ "0".
Compute strToZ "-0".
Compute strToZ "-3".

Compute String.index 0 "E" "123456E". 
*)


Definition splitAtExponent (s: string) : (string * string) :=
  let maybeEpos :=
  match String.index 0 "E" s with
  | Some epos => Some epos
  | None => match String.index 0 "e" s with
           | Some epos => Some epos
           | None => None
           end
  end in
  match maybeEpos with
  | Some epos => (substring 0 epos s, substring (S epos) (String.length s - epos - 1) s)
  | None => (s, EmptyString)
  end.

(*
Compute splitAtExponent "123E456".
Compute splitAtExponent "123E-456".
Compute splitAtExponent "123456".
*)

Definition splitAtPoint (s: string) : (string * string) :=
  match String.index 0 "." s with
  | Some dpos => (substring 0 dpos s, substring (S dpos) (String.length s - dpos - 1) s)
  | None => (s, EmptyString)
  end.

(*
Compute splitAtPoint "123.456".
Compute splitAtPoint "123.-456".
Compute splitAtPoint "123456".
*)

Definition decomposeFloatString  (s: string): (string * string * string) :=
  let (frac, exp) := splitAtExponent s in
  let (intPart, fracPart) := splitAtPoint frac in
  (intPart, fracPart, exp).

(* Compute decomposeFloatString "-123.456e-12". *)

Definition strToFloatHelper (s: string) : option (Z * Z) :=
  let '(intPart, fracPart, exp) := decomposeFloatString s in
  let mantstring := intPart ++ fracPart in
  zmant <- (strToZ mantstring);
    zexp <- (strToZ exp);
    (let adjustedZexp := zexp - Z.of_nat (String.length fracPart) in
     ret (zmant, adjustedZexp)).

(* Compute strToFloatHelper "-123.456e-12". *)

Definition my_nanpl := IEEE754.Bits.default_nan_pl64.
Definition my_nan : float := proj1_sig my_nanpl.

Definition strToFloat (s: string) : float :=
  match strToFloatHelper s with
  | Some (zmant, adjustedZexp) =>
    match zmant with
    | Z0 => Float.zero
    | Zpos x => from_parsed 10 x adjustedZexp
    | Zneg x => neg (from_parsed 10 x adjustedZexp)
    end
  | None => my_nan
  end.

(* Compute strToFloat "-123.456e-12". *)

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

Definition zero_to_string_unpadded (fdigs: nat) := 
  let digs_after_dec := pred fdigs in
  let frac := repeat_string digs_after_dec "0" in
  "0." ++ frac ++ "e+00".

Definition float_to_string_unpadded (x: float) (fdigs: nat) :=
  match x with
  | B754_zero false => "" ++ zero_to_string_unpadded fdigs
  | B754_zero true => "-" ++ zero_to_string_unpadded fdigs
  | B754_infinity false => "inf"
  | B754_infinity true => "-inf"
  | B754_nan false pl _ => "nan" ++ (Z_to_string_base16 14 (Zpos pl))
  | B754_nan true pl _ => "-nan" ++ (Z_to_string_base16 14 (Zpos pl))
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

Module DScopeNotations.

(*+ Float scope notations *)
(* D for double float *)
Bind Scope D_scope with float.
Delimit Scope D_scope with D.

Infix "+" := Float.add : D_scope.
Notation "- x" := (Float.neg x) : D_scope.
Infix "-" := Float.sub : D_scope.
Infix "*" := Float.mul : D_scope.
Infix "/" := Float.div : D_scope.
Infix "?=" := Float.compare (at level 70, no associativity) : D_scope.

Infix "=?" := (Float.cmp Ceq) (at level 70, no associativity) : D_scope.
Infix "<=?" := (Float.cmp Cle) (at level 70, no associativity) : D_scope.
Infix "<?" := (Float.cmp Clt) (at level 70, no associativity) : D_scope.
Infix ">=?" := (Float.cmp Cge) (at level 70, no associativity) : D_scope.
Infix ">?" := (Float.cmp Cgt) (at level 70, no associativity) : D_scope.
Notation "0" := Float.zero : D_scope.
Notation "float_string #D" := (Details.strToFloat float_string) (at level 10) : D_scope.
End DScopeNotations.

Import DScopeNotations.

(* Export items from the details module that the user will want. *)
Definition Z_to_string_base10 := Details.Z_to_string_base10.
Definition pad_to_width := Details.pad_to_width.
Definition float_to_string := Details.float_to_string.
Definition float_list_to_string := Details.float_list_to_string.
Definition strToFloat := FloatIO.Details.strToFloat.
Definition ZofFloat (f: float) :=
  match IEEE754_extra.ZofB 53 1024 f with
  | Some z => z
  | None => 0%Z
  end.


Definition round (f: float) : float :=
  let z := ZofFloat (f + "0.5"#D)%D in
  Z_to_float z.
Search b64_sqrt.
Definition sqrt (arg: float) : float :=
  IEEE754.Bits.b64_sqrt mode_NE arg.

End FloatIO.
