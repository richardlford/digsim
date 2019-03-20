open BinNums
open Datatypes
open Floats
open Float_text_io

module DebugIO :
 sig
  val width : nat

  val fdigs : nat

  val print_float : float -> char list

  val print_Z : coq_Z -> nat -> char list
 end
