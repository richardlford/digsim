open BinInt
open BinNums
open Coqlib
open Datatypes
open Zpower

type comparison =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module Wordsize_64 :
 sig
  val wordsize : nat
 end

module Int64 :
 sig
  val wordsize : nat

  val modulus : coq_Z

  val half_modulus : coq_Z

  val max_signed : coq_Z

  val min_signed : coq_Z

  type int = coq_Z
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> coq_Z

  val coq_P_mod_two_p : positive -> nat -> coq_Z

  val coq_Z_mod_modulus : coq_Z -> coq_Z

  val unsigned : int -> coq_Z

  val signed : int -> coq_Z

  val repr : coq_Z -> int
 end
