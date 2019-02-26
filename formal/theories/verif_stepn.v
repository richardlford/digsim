(* Verify simple double floating add. *)
Require Import VST.floyd.proofauto.
Require Import Digsim.stepn.
Require Import compcert.lib.Floats.

(* The next line is "boilerplate", always required after importing an AST. *)
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

