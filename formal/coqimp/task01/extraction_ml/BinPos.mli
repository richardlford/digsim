open BinNums
open Datatypes
open Decimal

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  val mul : positive -> positive -> positive

  val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1

  val square : positive -> positive

  val div2 : positive -> positive

  val div2_up : positive -> positive

  val size : positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val eqb : positive -> positive -> bool

  val coq_lor : positive -> positive -> positive

  val of_succ_nat : nat -> positive

  val of_uint_acc : uint -> positive -> positive

  val of_uint : uint -> coq_N

  val eq_dec : positive -> positive -> bool
 end
