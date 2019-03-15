(** * Verif_hash: Correctness proof of hash.c *)

Require Import VST.floyd.proofauto.
Require Import VST.floyd.library.
Require Import Task1.state_list.

Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

(*
  We define versions of the specs that take the identifiers and type arguments so that
  We can be imported into different separate compilation units. 

Definition strchr_spec' {CSX: compspecs} (_strchr _str _c: positive) :=
 DECLARE _strchr
  WITH sh: share, str : val, s : list byte, c : byte
  PRE  [ _str OF tptr tschar, _c OF tint ]
    PROP (readable_share sh; c <> Byte.zero)
    LOCAL (temp _str str; temp _c (Vbyte c))
    SEP (cstring sh s str)
  POST [ tptr tschar ]
   EX r : val,
    PROP ((exists i, Znth i s = c /\ Forall (fun d => d<>c) (sublist 0 i s)
                     /\ r = offset_val i str)
       \/ (Forall (fun d => d<>c) s /\ r = nullval))
    LOCAL (temp ret_temp r)
    SEP (cstring sh s str).

 *)

(*+ Definitions for State structure +*)

(* type of the state cell *)
Definition tstate := Tstruct _s_state noattr.

(* Type of pointer to state *)
Definition pstate := tptr tstate.

(* Field ident holding the array of items *)
Definition item_id := _item.

(* Coq type representing the state *)
Definition rstate := Eval compute in reptype tstate.

(* Define what it means for a pointer to hold a state cell *)
Definition state_cell (data: rstate) (p: val): mpred :=
 data_at Ews tstate data p * malloc_token Ews tstate p.

Definition state_cell_local_facts:
  forall data p, state_cell data p |-- !! isptr p.
Proof. intros. unfold state_cell. entailer!. Qed.
Hint Resolve state_cell_local_facts: saturate_local.

Definition state_cell_valid_pointer:
  forall data p, state_cell data p |-- valid_pointer p.
Proof. intros. unfold state_cell. entailer!. Qed.
Hint Resolve state_cell_valid_pointer: valid_pointer.

Lemma state_cell_fold: forall data p,
    data_at Ews tstate data p
    * malloc_token Ews tstate p
                   |-- state_cell data p.
Proof.
  intros.
  unfold state_cell.
  entailer!.
Qed.

(*+ * Definitions for list cells +*)

(* type of the list cell *)
Definition tcell := Tstruct _s_state_list noattr.

(* Field ident holding the data. *)
Definition data_id := _data.

(* Field ident holding the pointer to next cell *)
Definition next_id := _next.

(* Type of the data *)
Definition tdata := Tstruct _s_state noattr.

(* Type of pointer to data *)
Definition pdata := tptr tdata.

(* Coq type representing tdata *)
Definition rdata := Eval compute in reptype tdata.

(* Coq type representing tcell *)
Definition rcell := Eval compute in reptype tcell.

(* Define what it means for a pointer to hold a list cell *)
Definition list_cell (data: rstate) (next: val) (p: val): mpred :=
  EX datap: val,
            data_at Ews tcell (datap, next) p *
            malloc_token Ews tcell p *
            state_cell data datap.

Definition list_cell_local_facts:
  forall data next p, list_cell data next p |-- !! isptr p.
Proof.
  intros.
  unfold list_cell.
  entailer.
  unfold state_cell.
  entailer.
Qed.
Hint Resolve list_cell_local_facts: saturate_local.

Definition list_cell_valid_pointer:
  forall datap next p, list_cell datap next p |-- valid_pointer p.
Proof.
  intros.
  unfold list_cell.
  entailer.
Qed.
Hint Resolve list_cell_valid_pointer: valid_pointer.

Lemma listcell_fold: forall data datap p' p,
    data_at Ews tcell (datap, p') p *
    malloc_token Ews tcell p *
    state_cell data datap
                   |-- list_cell data p' p.
Proof.
  intros.
  unfold list_cell.
  Exists datap.
  entailer.
Qed.

Fixpoint listrep (sigma: list rdata) (x: val) : mpred :=
 match sigma with
 | d::hs => EX y: val, list_cell d y x * listrep hs y
 | nil => !! (x = nullval) && emp
 end.

Lemma listrep_local_prop: forall sigma p, listrep sigma p |--
        !! (is_pointer_or_null p  /\ (p=nullval <-> sigma=nil)).
Proof.
  intro sigma.
  induction sigma as [| d tl].
  { (* sigma is nil *)
    simpl.
    entailer!.
    split; reflexivity.
  }
  { (* sigma is d :: tl *)
    intro p'.
    simpl.
    entailer.
    saturate_local.
    entailer!.
    split.
    {
      intro.
      subst p'.
      contradiction.
    }
    {
      intro.
      inversion H0.
    }
  }
Qed.

Hint Resolve listrep_local_prop : saturate_local.

Lemma listrep_valid_pointer:
  forall sigma p, listrep sigma p |-- valid_pointer p.
Proof.
  intro sigma.
  induction sigma as [| d tl].
  { (* sigma is nil *)
    simpl.
    entailer!.
  }
  { (* sigma is d :: tl *)
    intro p'.
    simpl.
    entailer.
  }
Qed.

Hint Resolve listrep_valid_pointer : valid_pointer.

Definition copy_state_id := _copy_state.
Definition copy_state_arg0_id := _dest.
Definition copy_state_arg1_id := _src.

Definition copy_state_spec : ident * funspec :=
 DECLARE copy_state_id
 WITH shsrc: share, shdest: share, 
      datasrc: rdata, deaddata: rdata, 
      srcp: val, destp: val, gv: globals
 PRE [ copy_state_arg0_id OF pstate,
       copy_state_arg1_id OF pstate ]
   PROP(readable_share shsrc; writable_share shdest)
   LOCAL(temp copy_state_arg0_id destp;
           temp copy_state_arg1_id srcp;
           gvars gv)
   SEP(mem_mgr gv;
         state_cell datasrc srcp;
         state_cell deaddata destp
      )
 POST [ tvoid ]
      PROP()
      LOCAL()
      SEP(state_cell datasrc srcp; state_cell datasrc destp; mem_mgr gv).

Definition Gprog : funspecs :=
        ltac:(with_library prog [
                   copy_state_spec 
 ]).

Lemma body_copy_state: semax_body Vprog Gprog f_copy_state copy_state_spec.
Proof.
  start_function.
  forward.
  forward_while
    (exp (fun i:Z =>
        (PROP(0 <= i <= 3)
         LOCAL(temp _i (Vint (Int.repr i));
               temp copy_state_arg0_id destp;
               temp copy_state_arg1_id srcp;
               gvars gv)
         SEP(mem_mgr gv;
             state_cell datasrc srcp;
             state_cell
               ((sublist 0 i datasrc) ++ (sublist i 3 deaddata))
               destp)
        ))).

  forward_while
  
  forward_for_simple_bound 3
     (EX: i,
        PROP()
        LOCAL(temp copy_state_arg0_id destp; temp copy_state_arg1_id srcp; 
              gvars gv)
        SEP(mem_mgr gv; state_cell datasrc srcp; 
            state_cell deaddata destp)
     ).
  exp
  check_Delta.
  check_POSTCONDITION.
 repeat match goal with |-
      semax _ _ (Ssequence (Ssequence (Ssequence _ _) _) _) _ =>
      apply -> seq_assoc; abbreviate_semax
 end.
 match goal with |-
      semax _ _ (Ssequence (Ssequence (Sfor _ _ _ _) _) _) _ =>
      apply -> seq_assoc; abbreviate_semax
 | _ => idtac
 end.
 Check PROP()
        LOCAL(temp copy_state_arg0_id destp; temp copy_state_arg1_id srcp; 
              gvars gv)
        SEP(mem_mgr gv; state_cell datasrc srcp; 
            state_cell deaddata destp)%assert.
 
 first [
    match type of n with
      ?t => first [ unify t Z | elimtype (Type_of_bound_in_forward_for_should_be_Z_but_is t)]
    end;
    match type of Pre with
      ?t => first [unify t (environ -> mpred); fail 1 | elimtype (Type_of_invariant_in_forward_for_should_be_environ_arrow_mpred_but_is t)]
    end
  | match goal with
    | |- semax _ _ (Sfor _ _ _ _) _ =>
           rewrite semax_seq_skip
    | |- semax _ _ (Ssequence _ (Sloop _ _)) _ =>
           rewrite semax_seq_skip
    | |- semax _ _ (Ssequence _ ?MORE_COMMANDS) _ =>
        revert MORE_COMMANDS;
        match goal with
        | |- let MORE_COMMANDS := @abbreviate _ (Sloop _ _) in _ =>
            intros MORE_COMMANDS;
            rewrite semax_seq_skip
        end
    | _ => idtac
    end;
    forward_for_simple_bound'' n Pre; [.. | abbreviate_semax; cbv beta; try fwd_skip]
  ].
  forward_for_simple_bound 3
     (EX: i,
        PROP()
        LOCAL(temp copy_state_arg0_id destp; temp copy_state_arg1_id srcp; 
              gvars gv)
        SEP(mem_mgr gv; state_cell datasrc srcp; 
            state_cell deaddata destp)
     )%assert.
  
Qed.

Definition clone_state_id := _clone_state.
Definition clone_state_arg0_id := _data.

Definition clone_state_spec : ident * funspec :=
 DECLARE clone_state_id
 WITH sh: share, data: rdata, datap: val, gv: globals
 PRE [ clone_state_arg0_id OF pstate ]
   PROP(readable_share sh)
   LOCAL(temp clone_state_arg0_id datap; gvars gv)
   SEP(mem_mgr gv; state_cell data datap)
 POST [ pstate ]
   EX p:val, PROP()
      LOCAL(temp ret_temp p)
      SEP(state_cell data datap; state_cell data p; mem_mgr gv).


(*+  Putting all the funspecs together +*)

Definition Gprog : funspecs :=
        ltac:(with_library prog [
                   copy_state_spec clone_state_spec
 ]).


(*+ Body Proofs +*)

Lemma body_copy_state: semax_body Vprog Gprog f_copy_state copy_state_spec.
Proof.
  start_function.
Qed.



Lemma body_clone_state: semax_body Vprog Gprog f_clone_state clone_state_spec.
Proof.
  start_function.
Qed.



Definition new_cell_id := _new_state_list.
Definition new_cell_spec : ident * funspec :=
 DECLARE new_cell_id
 WITH sh: share, data: rdata, datap: val, next: val, gv: globals
 PRE [ data_id OF tdata, next_id OF tptr tcell ]
   PROP(readable_share sh)
   LOCAL(temp data_id val; temp next_id next; gvars gv)
   SEP(mem_mgr gv)
 POST [ tptr tcell ]
   EX p:val, PROP()
      LOCAL(temp ret_temp p)
      SEP(list_cell data next p; mem_mgr gv).




