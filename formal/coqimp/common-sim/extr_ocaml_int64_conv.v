(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Extraction to Ocaml: conversion from/to [int64]

    Nota: no check that [int64] values aren't generating overflows *)

Require Coq.extraction.Extraction.

Require Import Arith ZArith.

Parameter int64 : Type.
Parameter int64_zero : int64.
Parameter int64_succ : int64 -> int64.
Parameter int64_opp : int64 -> int64.
Parameter int64_twice : int64 -> int64.

Extract Inlined Constant int64 => int64.
Extract Inlined Constant int64_zero => "Int64.zero".
Extract Inlined Constant int64_succ => "(Int64.add 1L)".
Extract Inlined Constant int64_opp => "Int64.neg".
Extract Inlined Constant int64_twice => "(Int64.mul 2L)".

Definition int64_of_nat : nat -> int64 :=
 (fix loop acc n :=
  match n with
   | O => acc
   | S n => loop (int64_succ acc) n
  end) int64_zero.

Fixpoint int64_of_pos p :=
 match p with
  | xH => int64_succ int64_zero
  | xO p => int64_twice (int64_of_pos p)
  | xI p => int64_succ (int64_twice (int64_of_pos p))
 end.

Fixpoint int64_of_z z :=
 match z with
  | Z0 => int64_zero
  | Zpos p => int64_of_pos p
  | Zneg p => int64_opp (int64_of_pos p)
 end.

Fixpoint int64_of_n n :=
 match n with
  | N0 => int64_zero
  | Npos p => int64_of_pos p
 end.

(** NB: as for [pred] or [minus], [nat_of_int64], [n_of_int64] and
    [pos_of_int64] are total and return zero (resp. one) for
    non-positive inputs. *)

Parameter int64_natlike_rec : forall A, A -> (A->A) -> int64 -> A.
Extract Constant int64_natlike_rec =>
"fun fO fS ->
 let rec loop acc (i:int64) = if i <= 0L then acc else loop (fS acc) (Int64.sub i 1L)
 in loop fO".

Definition nat_of_int64 : int64 -> nat := int64_natlike_rec _ O S.

Parameter int64_poslike_rec : forall A, A -> (A->A) -> (A->A) -> int64 -> A.
Extract Constant int64_poslike_rec =>
"fun f1 f2x f2x1 ->
 let rec loop (i:int64) = if i <= 1L then f1 else
  if (Int64.logand i 1L) = 0L then f2x (loop (Int64.shift_right_logical i 1)) else f2x1 (loop (Int64.shift_right_logical i 1))
 in loop".

Definition pos_of_int64 : int64 -> positive := int64_poslike_rec _ xH xO xI.

Parameter int64_zlike_case : forall A, A -> (int64->A) -> (int64->A) -> int64 -> A.
Extract Constant int64_zlike_case =>
"fun f0 fpos fneg i ->
 if i = 0L then f0 else if i>0L then fpos i else fneg (Int64.neg i)".

Definition z_of_int64 : int64 -> Z :=
 int64_zlike_case _ Z0 (fun i => Zpos (pos_of_int64 i))
                     (fun i => Zneg (pos_of_int64 i)).

Definition n_of_int64 : int64 -> N :=
 int64_zlike_case _ N0 (fun i => Npos (pos_of_int64 i)) (fun _ => N0).

(** Warning: [z_of_int64] is currently wrong for Ocaml's [min_int64],
    since [min_int64] has no positive opposite ([-min_int64 = min_int64]).
*)

(*
Extraction "/tmp/test.ml"
  nat_of_int64 int64_of_nat
  pos_of_int64 int64_of_pos
  z_of_int64 int64_of_z
  n_of_int64 int64_of_n.
*)
