(** * Verif_hash: Correctness proof of hash.c *)

Require Import VST.floyd.proofauto.
Require Import VST.floyd.library.
Require Import  hash.
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

Require Import VST.msl.wand_frame.
Require Import VST.msl.iter_sepcon.
Require Import VST.floyd.reassoc_seq.
Require Import VST.floyd.field_at_wand.
Require Import Hashfun.
