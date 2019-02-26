(* Verify simple double floating add. *)
Require Import VST.floyd.proofauto.
Require Import Digsim.step.
Require Import compcert.lib.Floats.

(* The next line is "boilerplate", always required after importing an AST. *)
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

Definition t_struct_state := Tstruct _s_state noattr.

Record state : Set :=
  mkState 
    { t : float
      ; x : float
      ; xd : float
    }.

Definition state_tuple (s : state) : val * (val * val) :=
  let t' := Vfloat s.(t) in
  let x' := Vfloat s.(x) in
  let xd' := Vfloat s.(t) in
  (t', (x', xd')).

Eval compute in reptype t_struct_state.
      
Definition state_rep (s : state) (p: val) : mpred :=
  data_at Tsh t_struct_state (state_tuple s) p.

Definition zero := Float.zero.

Definition s0 := {| t := zero; x := zero; xd := zero |}.
Eval compute in s0.(t).
Eval compute in t s0.

(* In this section we abstract over the semantics of the function that
   computes the acceleration.
*)
Section XDD_Abstract.
  Variable acceleration : state -> float.

  Definition oneStep (input: state) (dt: float) : state :=
    let xdd := acceleration input in
    let t' := Float.add input.(t) dt in
    let x' := Float.add input.(x) (Float.mul input.(xd) dt) in
    let xd' := Float.add input.(xd) (Float.mul xdd dt) in
    {| t := t'; x := x'; xd := xd' |}.

  Definition xddp_spec :=
    WITH tf: float, xf: float, xdf: float
    PRE [ _t OF tdouble, _x OF tdouble, _xd OF tdouble]
            PROP ()
            LOCAL (temp _t (Vfloat tf); temp _x (Vfloat xf); temp _xd (Vfloat xdf))
            SEP ()
    POST [ tdouble ]
            PROP() 
            LOCAL (temp ret_temp (Vfloat (acceleration {| t := tf; x := xf; xd := xdf |}))) 
            SEP().

  Definition xddp_type :=
    (tptr (Tfunction
             (Tcons tdouble (Tcons tdouble (Tcons tdouble Tnil)))
                         tdouble cc_default)).
  
  Definition step_spec :=
    DECLARE _step
      WITH s: state, dt: float, stp: val, xddp: val
      PRE [ _stp OF tptr t_struct_state, 
            _xddp OF xddp_type,
            _dt OF tdouble]
      PROP( )
      LOCAL(temp _stp stp; temp _xddp xddp; temp _dt (Vfloat dt))
      SEP (state_rep s stp; func_ptr' xddp_spec xddp)
      POST [ tvoid ] 
      PROP()
      LOCAL()
      SEP (state_rep (oneStep s dt) stp; func_ptr' xddp_spec xddp).

  Definition Gprog : funspecs :=
    ltac:(with_library prog [ step_spec ]).


  Lemma body_step: semax_body Vprog Gprog f_step step_spec.
  Proof.
    start_function.
    unfold state_rep.
    unfold_data_at (data_at _ _ _ stp).
    forward.
    forward.
    forward.
    fwd_call subsume_funspec_refl (t s, x s, xd s).
    {
      admit.
    }
    {
      forward.
      forward.
      forward.
      forward.
      forward.
      forward.
      forward.
      forward.
      forward.
      entailer!.
      unfold oneStep.
      unfold state_rep.
      unfold_data_at (data_at _ _ _ stp).
      simpl.
      entailer!.
    }
    
  Qed.
  
End XDD_Abstract
