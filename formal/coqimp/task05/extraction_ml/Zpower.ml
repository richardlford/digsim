open BinNums
open BinPos
open Datatypes

(** val shift_nat : nat -> positive -> positive **)

let rec shift_nat n z =
  match n with
  | O -> z
  | S n0 -> Coq_xO (shift_nat n0 z)

(** val shift_pos : positive -> positive -> positive **)

let shift_pos n z =
  Pos.iter (fun x -> Coq_xO x) z n

(** val two_power_nat : nat -> coq_Z **)

let two_power_nat n =
  Zpos (shift_nat n Coq_xH)
