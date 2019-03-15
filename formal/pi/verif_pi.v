(* Verify simple double floating add. *)
Require Import VST.floyd.proofauto.
Require Import Pi.pi.
(* The next line is "boilerplate", always required after importing an AST. *)
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

Definition pi_spec :=
  DECLARE _pi
    WITH z : Z
    PRE [ ]
    PROP( )
    LOCAL()
    SEP ()
  POST [ tdouble ] 
    PROP()
    LOCAL(temp ret_temp (Vfloat (Float.of_bits (Int64.repr 4614256656552045848))))
    SEP ().

Definition Gprog : funspecs :=
    ltac:(with_library prog [
    pi_spec
  ]).

Lemma body_pi: semax_body Vprog Gprog f_pi pi_spec.
Proof.
  start_function.
  forward.
  forward.
Qed.


