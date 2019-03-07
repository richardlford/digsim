(* Verify simple double floating add. *)
Require Import VST.floyd.proofauto.
Require Import Step0.step0.
Require Import compcert.lib.Floats.

(* The next line is "boilerplate", always required after importing an AST. *)
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

(* In this section we abstract over the semantics of the function that
   computes the delta.
*)
Section XD_Abstract.
  Variable delta : float -> float.

  Definition oneStep0 (input: float) : float :=
    let xd := delta input in
    Float.add input xd.

  Definition xdp_spec :=
    WITH xf: float
    PRE [ _x OF tdouble]
            PROP ()
            LOCAL (temp _x (Vfloat xf))
            SEP ()
    POST [ tdouble ]
            PROP() 
            LOCAL (temp ret_temp (Vfloat (delta xf))) 
            SEP().

  Definition xdp_type :=
    (tptr (Tfunction (Tcons tdouble Tnil) tdouble cc_default)).
  
  Definition step0_spec :=
    DECLARE _step0
      WITH x: float, xdp: val
      PRE [ _x OF tdouble,
            _xdp OF xdp_type]
      PROP( )
      LOCAL(temp _x (Vfloat x); temp _xdp xdp)
      SEP (func_ptr' xdp_spec xdp)
      POST [ tdouble ] 
      PROP()
      LOCAL(temp ret_temp (Vfloat (oneStep0 x)))
      SEP (func_ptr' xdp_spec xdp).

  Definition Gprog : funspecs :=
    ltac:(with_library prog [ step0_spec ]).


  Lemma body_step0: semax_body Vprog Gprog f_step0 step0_spec.
  Proof.
    start_function.
    forward_call x.
    forward.
    forward.
  Qed.
  
End XD_Abstract.
