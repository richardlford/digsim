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

(* type of the list cell *)
Definition tcell := Tstruct _s_state_list noattr.

(* Field ident holding the data. *)
Definition data_id := _data.

(* Field ident holding the pointer to next cell *)
Definition next_id := _next.

(* Type of the data *)
Definition tdata := Tstruct _s_state noattr.

(* Coq type representing tdata *)
Definition rdata := Eval compute in reptype tdata.

(* Coq type representing tcell *)
Definition rcell := Eval compute in reptype tcell.

(* Define what it means for a pointer to hold a list cell *)
Definition list_cell (data: rdata) (next: val) (p: val): mpred :=
 data_at Ews tcell (data, next) p * malloc_token Ews tcell p.

Definition list_cell_local_facts:
  forall data next p, list_cell data next p |-- !! isptr p.
Proof. intros. unfold list_cell. entailer!. Qed.
Hint Resolve list_cell_local_facts: saturate_local.

Definition list_cell_valid_pointer:
  forall data next p, list_cell data next p |-- valid_pointer p.
Proof. intros. unfold list_cell. entailer!. Qed.
Hint Resolve list_cell_valid_pointer: valid_pointer.

Lemma listcell_fold: forall data p' p,
    data_at Ews tcell (data, p') p
    * malloc_token Ews tcell p
                   |-- list_cell data p' p.
Proof.
  intros.
  unfold list_cell.
  entailer!.
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

Definition new_cell_id := _new_state_list.
Definition new_cell_spec : ident * funspec :=
 DECLARE new_cell_id
 WITH sh: share, data: rdata, next: val, gv: globals
 PRE [ data_id OF rdata, next_id OF tptr tcell ]
   PROP(readable_share sh)
   LOCAL(temp data_id data; temp next_id next; gvars gv)
   SEP(mem_mgr gv)
 POST [ tptr tcell ]
   EX p:val, PROP()
      LOCAL(temp ret_temp p)
      SEP(list_cell data next p; mem_mgr gv).




