(* Verify simple double floating add. *)
Require Import VST.floyd.proofauto.
Require Import Digsim.dadd.
(* The next line is "boilerplate", always required after importing an AST. *)
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

Definition dadd_spec :=
  DECLARE _dadd
    WITH a: float, b: float
    PRE [ _a OF tdouble, _b OF tdouble]
    PROP( )
    LOCAL(temp _a (Vfloat a); temp _b (Vfloat b))
    SEP ()
  POST [ tdouble ] 
    PROP()
    LOCAL(temp ret_temp (Vfloat (Float.add a b)))
    SEP ().

Definition Gprog : funspecs :=
    ltac:(with_library prog [
    dadd_spec
  ]).

Lemma body_dadd: semax_body Vprog Gprog f_dadd dadd_spec.
Proof.
  start_function.
  forward.
  forward.
Qed.


