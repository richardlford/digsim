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

module Wordsize_64 =
 struct
  (** val wordsize : nat **)

  let wordsize =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 end

module Int64 =
 struct
  (** val wordsize : nat **)

  let wordsize =
    Wordsize_64.wordsize

  (** val modulus : coq_Z **)

  let modulus =
    two_power_nat wordsize

  (** val half_modulus : coq_Z **)

  let half_modulus =
    Z.div modulus (Zpos (Coq_xO Coq_xH))

  (** val max_signed : coq_Z **)

  let max_signed =
    Z.sub half_modulus (Zpos Coq_xH)

  (** val min_signed : coq_Z **)

  let min_signed =
    Z.opp half_modulus

  type int = coq_Z
    (* singleton inductive, whose constructor was mkint *)

  (** val intval : int -> coq_Z **)

  let intval i =
    i

  (** val coq_P_mod_two_p : positive -> nat -> coq_Z **)

  let rec coq_P_mod_two_p p = function
  | O -> Z0
  | S m ->
    (match p with
     | Coq_xI q -> Z.succ_double (coq_P_mod_two_p q m)
     | Coq_xO q -> Z.double (coq_P_mod_two_p q m)
     | Coq_xH -> Zpos Coq_xH)

  (** val coq_Z_mod_modulus : coq_Z -> coq_Z **)

  let coq_Z_mod_modulus = function
  | Z0 -> Z0
  | Zpos p -> coq_P_mod_two_p p wordsize
  | Zneg p ->
    let r = coq_P_mod_two_p p wordsize in
    if zeq r Z0 then Z0 else Z.sub modulus r

  (** val unsigned : int -> coq_Z **)

  let unsigned =
    intval

  (** val signed : int -> coq_Z **)

  let signed n =
    let x = unsigned n in if zlt x half_modulus then x else Z.sub x modulus

  (** val repr : coq_Z -> int **)

  let repr =
    coq_Z_mod_modulus
 end
