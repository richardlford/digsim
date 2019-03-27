open BinInt
open BinNums
open Datatypes
open Decimal
open Fappli_IEEE
open Fappli_IEEE_extra
open Floats
open Integers
open Monad
open Nat
open String0

module FloatIO :
 sig
  val coq_Z_to_float : coq_Z -> float

  module Details :
   sig
    val precision_of_float : coq_Z

    val log10of2scaled3 : coq_Z

    val strToUintHelper : char list -> uint option

    val strToUint : char list -> uint option

    val strToIntHelper : char list -> int option

    val strToInt : char list -> int option

    val strToZ : char list -> coq_Z option

    val splitAtExponent : char list -> char list * char list

    val splitAtPoint : char list -> char list * char list

    val decomposeFloatString : char list -> (char list * char list) * char list

    val strToFloatHelper : char list -> (coq_Z * coq_Z) option

    val strToFloat : char list -> float option

    val scale_exp : coq_Z -> coq_Z -> coq_Z

    val digits : coq_Z -> char list

    val repeat_string : nat -> char list -> char list

    val coq_Z_to_string_base10_aux : nat -> nat -> coq_Z -> char list

    val coq_Z_to_string_base10 : nat -> coq_Z -> char list

    val scaled_float_to_Z : float -> coq_Z -> coq_Z

    val insert_decimal : char list -> nat -> char list

    val float_to_string_unsigned : float -> nat -> char list

    val float_to_string_unpadded : float -> nat -> char list

    val pad_to_width : nat -> char list -> char list

    val float_to_string : nat -> nat -> float -> char list
   end

  val coq_Z_to_string_base10 : nat -> coq_Z -> char list

  val float_to_string : nat -> nat -> float -> char list

  val strToFloat : char list -> float option

  val strToFloat' : char list -> float

  val coq_ZofFloat : float -> coq_Z

  val fhalf_in_floatio : float

  val round : float -> float
 end
