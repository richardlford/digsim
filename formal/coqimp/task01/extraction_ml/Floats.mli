open Archi
open BinNums
open BinPos
open Coqlib
open Datatypes
open Fappli_IEEE
open Fappli_IEEE_bits
open Fappli_IEEE_extra
open Fcore_Zaux
open Integers

type float = binary64

val cmp_of_comparison : comparison -> Datatypes.comparison option -> bool

module Float :
 sig
  val transform_quiet_pl : nan_pl -> nan_pl

  val neg_pl : bool -> nan_pl -> bool * nan_pl

  val abs_pl : bool -> nan_pl -> bool * nan_pl

  val binop_pl : binary64 -> binary64 -> bool * nan_pl

  val zero : float

  val neg : float -> float

  val abs : float -> float

  val add : float -> float -> float

  val sub : float -> float -> float

  val mul : float -> float -> float

  val div : float -> float -> float

  val compare : float -> float -> Datatypes.comparison option

  val cmp : comparison -> float -> float -> bool

  val to_long : float -> Int64.int option

  val from_parsed : positive -> positive -> coq_Z -> float

  val to_bits : float -> Int64.int

  val of_bits : Int64.int -> float
 end
