(** * Verif_hash: Correctness proof of hash.c *)

Require Import VST.floyd.proofauto.
Require Import VST.floyd.library.
Require Import  Hash.hash.
Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

Require Import VST.msl.wand_frame.
Require Import VST.msl.iter_sepcon.
Require Import VST.floyd.reassoc_seq.
Require Import VST.floyd.field_at_wand.
Require Import Hash.Hashfun.

(* ================================================================= *)
(** ** Function specifications *)
(* ----------------------------------------------------------------- *)
(** *** Imports from the C string library *)

Require Import DigUty.strlib_specs.
Definition strcmp_spec := strcmp_spec' _strcmp 1 2.
Definition strcpy_spec := strcpy_spec' _strcpy 1 2.
Definition strlen_spec := strlen_spec' _strlen 1.

(* ----------------------------------------------------------------- *)
(** ***  String functions:  copy, hash *)

Definition copy_string_spec : ident * funspec :=
 DECLARE _copy_string
 WITH sh: share, s: val, sigma : string, gv: globals
 PRE [ _s OF tptr tschar ]
    PROP (readable_share sh; Zlength sigma < Int.max_signed)
    LOCAL (temp _s s; gvars gv)
    SEP (mem_mgr gv; cstring sh sigma s)
 POST [ tptr tschar ]
    EX p: val, PROP ( ) LOCAL (temp ret_temp p)
               SEP (cstring sh sigma s; cstring Ews sigma p;
                      mem_mgr gv;
                      malloc_token Ews (tarray tschar (Zlength sigma + 1)) p).

Definition hash_spec : ident * funspec :=
  DECLARE _hash
  WITH sh: share, s: val, contents : string
  PRE [ _s OF (tptr tschar) ]
          PROP  (readable_share sh; Zlength contents < Int.max_signed)
          LOCAL (temp _s s)
          SEP   (cstring sh contents s)
  POST [ tuint ]
        PROP ()
	LOCAL(temp ret_temp  (Vint (Int.repr (hashfun contents))))
        SEP (cstring sh contents s).

(* ----------------------------------------------------------------- *)
(** *** Data structures for hash table *)

Definition tcell := Tstruct _cell noattr.
Definition thashtable := Tstruct _hashtable noattr.

Definition list_cell (key: string) (count: Z) (next: val) (p: val): mpred :=
 EX kp: val, cstring Ews key kp * data_at Ews tcell (kp,(Vint (Int.repr count), next)) p
             * malloc_token Ews tcell p
             * malloc_token Ews (tarray tschar (Zlength key + 1)) kp.

Definition list_cell_local_facts:
  forall key count next p, list_cell key count next p |-- !! isptr p.
Proof. intros. unfold list_cell. Intros kp. entailer!. Qed.
Hint Resolve list_cell_local_facts: saturate_local.

Definition list_cell_valid_pointer:
  forall key count next p, list_cell key count next p |-- valid_pointer p.
Proof. intros. unfold list_cell. Intros kp. entailer!. Qed.
Hint Resolve list_cell_valid_pointer: valid_pointer.

(** **** Exercise: 1 star (listcell_fold)  *)
Lemma listcell_fold: forall key kp count p' p,
    cstring Ews key kp * data_at Ews tcell (kp, (Vint (Int.repr count), p')) p
    * malloc_token Ews tcell p
    * malloc_token Ews (tarray tschar (Zlength key + 1)) kp
         |-- list_cell key count p' p.
Proof.
  intros.
  unfold list_cell.
  Exists kp.
  entailer!.
Qed.

(** [] *)

Fixpoint listrep (sigma: list (string * Z)) (x: val) : mpred :=
 match sigma with
 | (s,c)::hs => EX y: val, list_cell s c y x * listrep hs y
 | nil =>
    !! (x = nullval) && emp
 end.

(** **** Exercise: 2 stars (listrep_hints)  *)
Lemma listrep_local_prop: forall sigma p, listrep sigma p |--
        !! (is_pointer_or_null p  /\ (p=nullval <-> sigma=nil)).
Proof.
  intro sigma.
  induction sigma as [| [hkey hcount] tl].
  { (* sigma is nil *)
    simpl.
    entailer!.
    split; reflexivity.
  }
  { (* sigma is (hkey, hcount) :: tl *)
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
  forall sigma p,
   listrep sigma p |-- valid_pointer p.
Proof.
  intro sigma.
  induction sigma as [| [hkey hcount] tl].
  { (* sigma is nil *)
    simpl.
    entailer!.
  }
  { (* sigma is (hkey, hcount) :: tl *)
    intro p'.
    simpl.
    entailer.
  }
Qed.

Hint Resolve listrep_valid_pointer : valid_pointer.
(** [] *)

Lemma listrep_fold: forall key count p' p al,
  list_cell key count p' p * listrep al p' |-- listrep ((key,count)::al) p.
Proof. intros. simpl. Exists p'. cancel. Qed.

Lemma listrep_fold_nil: forall key count (p: val),
  list_cell key count nullval p |-- listrep ((key,count)::nil) p.
Proof.
  intros. simpl.
  Exists nullval.
  entailer!.
Qed.

Definition listboxrep al r :=
 EX p:val, data_at Tsh (tptr tcell) p r * listrep al p.

Definition uncurry {A B C} (f: A -> B -> C) (xy: A*B) : C :=
  f (fst xy) (snd xy).

Definition hashtable_rep (contents: hashtable_contents) (p: val) : mpred :=
  EX bl: list (list (string * Z) * val),
    !! (contents = map fst bl) &&
    malloc_token Ews thashtable p *
    field_at Ews thashtable [StructField _buckets] (map snd bl) p
    * iter_sepcon (uncurry listrep) bl.


(** **** Exercise: 2 stars (hashtable_rep_hints)  *)
Lemma hashtable_rep_local_facts: forall contents p,
 hashtable_rep contents p |-- !! (isptr p /\ Zlength contents = N).
Proof.
  intros.
  unfold hashtable_rep.
  entailer.
  saturate_local.
  hint.
  autorewrite with sublist.
  entailer!.
  rewrite N_eq.
  inversion H1.
  simpl in *.
  unfold unfold_reptype in H2.
  simpl in H2.
  rewrite Zlength_map in H2.
  auto.
Qed.

Hint Resolve hashtable_rep_local_facts : saturate_local.

Lemma hashtable_rep_valid_pointer: forall contents p,
 hashtable_rep contents p |-- valid_pointer p.
Proof.
  intros.
  unfold hashtable_rep.
  entailer.
Qed.


Hint Resolve hashtable_rep_valid_pointer : valid_pointer.
(** [] *)

(* ----------------------------------------------------------------- *)
(** *** Function specifications for hash table *)

Definition new_table_spec : ident * funspec :=
 DECLARE _new_table
 WITH u: unit, gv: globals
 PRE [ ]
   PROP()
   LOCAL(gvars gv)
   SEP(mem_mgr gv)
 POST [ tptr thashtable ]
   EX p:val, PROP()
      LOCAL(temp ret_temp p)
      SEP(mem_mgr gv; hashtable_rep empty_table p).

Definition new_cell_spec : ident * funspec :=
 DECLARE _new_cell
 WITH sh: share, s: val, key: string, count: Z, next: val, gv: globals
 PRE [ _key OF tptr tschar, _count OF tint, _next OF tptr tcell ]
   PROP(readable_share sh; Zlength key < Int.max_signed)
   LOCAL(temp _key s; temp _count (Vint (Int.repr count)); temp _next next; gvars gv)
   SEP(mem_mgr gv; cstring sh key s)
 POST [ tptr tcell ]
   EX p:val, PROP()
      LOCAL(temp ret_temp p)
      SEP(list_cell key count next p; cstring sh key s;
          mem_mgr gv).

Definition get_spec : ident * funspec :=
 DECLARE _get
 WITH sh: share, p: val, contents: hashtable_contents, s: val, sigma : string
 PRE [ _table OF tptr thashtable, _s OF tptr tschar ]
    PROP (readable_share sh; Zlength sigma < Int.max_signed)
    LOCAL (temp _table p; temp _s s)
    SEP (hashtable_rep contents p; cstring sh sigma s)
 POST [ tuint ]
      PROP ( ) LOCAL (temp ret_temp (Vint (Int.repr (hashtable_get sigma contents))))
      SEP (hashtable_rep contents p; cstring sh sigma s).

Definition incr_list_spec : ident * funspec :=
 DECLARE _incr_list
 WITH sh: share, r0: val, al: list (string * Z), s: val, sigma : string
 PRE [ _r0 OF tptr (tptr tcell), _s OF tptr tschar ]
    PROP (readable_share sh; Zlength sigma < Int.max_signed; list_get sigma al < Int.max_unsigned)
    LOCAL (temp _r0 r0; temp _s s)
    SEP (listboxrep al r0; cstring sh sigma s)
 POST [ tvoid ]
      PROP ( ) LOCAL ()
      SEP (listboxrep (list_incr sigma al) r0; cstring sh sigma s).

Definition incr_spec : ident * funspec :=
 DECLARE _incr
 WITH sh: share, p: val, contents: hashtable_contents, s: val, sigma : string
 PRE [ _table OF tptr thashtable, _s OF tptr tschar ]
    PROP (readable_share sh; Zlength sigma < Int.max_signed; 
          hashtable_get sigma contents < Int.max_unsigned)
    LOCAL (temp _table p; temp _s s)
    SEP (hashtable_rep contents p; cstring sh sigma s)
 POST [ tvoid ]
      PROP ( ) LOCAL ()
      SEP (hashtable_rep (hashtable_incr sigma  contents) p; cstring sh sigma s).

(**  Putting all the funspecs together *)

Definition Gprog : funspecs :=
        ltac:(with_library prog [
                   strcmp_spec; strcpy_spec; strlen_spec; hash_spec;
                   new_cell_spec; copy_string_spec; get_spec; incr_spec;
                   incr_list_spec
 ]).

(* ================================================================= *)
(** ** Proofs of the function bodies *)

(** This lemma demonstrates how to handle "strings", that is,
   null-terminated arrays of characters. *)
Lemma demonstrate_cstring1:
 forall i contents,
   ~ In Byte.zero contents ->
   Znth i (contents ++ [Byte.zero]) <> Byte.zero  ->
   0 <= i <= Zlength contents ->
   0 <= i + 1 < Zlength (contents ++ [Byte.zero]).
Proof.
intros.
(** When processing a C null-terminated string, you will want to
   maintain the three kinds of assumptions above the line.
   A string is an array of characters with three parts:
   - The "contents" of the string, none of which is the '\0' character;
   - The null termination character, equal to Byte.zero;
   - the remaining garbage in the array, after the null.
  The first assumption above the line says that none of the
  contents is the null character.
  Now suppose we are in a loop where variable [_i] (with value [i])
  is traversing the array.  We expect that loop to go up to but
  no farther than the null character, that is, one past the contents.
  Therefore [0 <= i <= Zlength contents].
  Furthermore, suppose we have determined (by an if-test) that
  s[i] is not zero, then we have the hypothesis H0 above.

  The [cstring] tactic processes all three of them to conclude
  that [i < Zlength contents]. *)
assert (H7: i < Zlength contents) by cstring.

(** But actually, [cstring] tactic will prove any rep_omega consequence
   of that fact.  For example: *)
clear H7.
autorewrite with sublist.
cstring.
Qed.
(** Don't apply [demonstrate_cstring1] in the body_hash proof,
    instead, use [autorewrite with sublist] and [cstring] directly,
    where appropriate. *)

Lemma demonstrate_cstring2:
 forall i contents,
   ~ In Byte.zero contents ->
   Znth i (contents ++ [Byte.zero]) = Byte.zero  ->
   0 <= i <= Zlength contents ->
   i = Zlength contents.
Proof.
intros.
(** Here is another demonstration.  When your loop on the
   string contents reaches the end, so that s[i] is the zero byte,
   you'll have the an assumption like [H0] above the line.
   The [cstring] tactic handles this case too, to prove
   [i = Zlength contents].   *)
cstring.
Qed.

(** **** Exercise: 3 stars (body_hash)  *)
Lemma body_hash: semax_body Vprog Gprog f_hash hash_spec.
Proof.
start_function.
unfold cstring,hashfun in *.
(** Before doing this proof, study some of the proofs in VST/progs/verif_strlib.v.
  In the PROP part of your loop invariant, you'll want to maintain [0 <= i <= Zlength contents].

  In the LOCAL part of your loop invariant, try to use something like

    temp _c (Vbyte (Znth i (contents ++ [Byte.zero]))

  instead of

    temp _c (Znth 0 (map Vbyte (...)))

  The reason is that [temp _c (Vint x)] or [temp _c (Vbyte y)] is much easier
  for Floyd to handle than [temp _c X]
  where X is a general formula of type [val].

  Late in the proof of the loop body, the lemma [hashfun_snoc] will be useful. *)
forward.
forward.
forward.
entailer!.
forward_while
  (EX i:Z,
    PROP (0 <= i < (Zlength contents + 1))
    LOCAL (temp _c (Vbyte (Znth i (contents ++ [Byte.zero])));
           temp _i (Vint (Int.repr i));
           temp _n (Vint (Int.repr (hashfun_aux 0 (sublist 0 i contents))));
           temp _s s)
    SEP (!! (~ In Byte.zero contents) &&
                 data_at sh (tarray tschar (Zlength contents + 1)) (map Vbyte (contents ++ [Byte.zero])) s)).
{ (* Current pre implies invariant. *)
  Exists 0.
  entailer!.
}
{ (* Type Check *)
  entailer!.
}
{ (* Now in the loop body. *)
  forward.
  forward.
  assert(i <> Zlength contents). {
    intro.
    subst i.
    simpl in HRE.
    rewrite app_Znth2 in HRE.
    rewrite Z.sub_diag in HRE.
    unfold Znth in HRE.
    simpl in HRE.
    contradiction.
    (* Now must prove Zlength contents >= Zlength contents *)
    omega.
  }
  assert(0 <= i < Zlength contents). {
    omega.
  }
  forward.
  Exists (i + 1).
  entailer!.
  f_equal.
  rewrite app_Znth1 by omega.
  apply (hashfun_snoc contents 0 0 i);omega.
}
{ (* At the return *)
  forward.
  entailer!.
  f_equal.
  assert(i = Zlength contents). {
    assert(~ (0 <= i < Zlength contents)). {
      intro.
      rewrite app_Znth1 in HRE by omega.
      assert(In (Znth i contents) contents). {
        apply Znth_In; auto.
      }
      rewrite HRE in H6.
      contradiction.
    }
    omega.
  }
  subst i.
  rewrite sublist_same by omega.
  reflexivity.
}
Qed.

(** [] *)

(** **** Exercise: 3 stars (body_new_cell)  *)

Lemma body_copy_string: semax_body Vprog Gprog f_copy_string copy_string_spec.
Proof.
  start_function.
  forward_call (sh, sigma, s). (* strlen *)
  forward.
  forward_call ((tarray tschar (Zlength sigma + 1)), gv). (* to malloc *)
  {
    (* Prove Zlength size + 1 in two forms are equal. *)
    entailer!.
    repeat f_equal.
    assert(Int.signed (Int.repr 1) = 1). {
      (* Search (Int.signed (Int.repr _)).*)
      apply Int.signed_repr.
      rep_omega.
    }
    rewrite H1.
    (* Search (Int64.unsigned (Int64.repr _)). *)
    assert((Int64.unsigned (Int64.repr (Zlength sigma + 1))) = (Zlength sigma + 1)). {
      apply Int64.unsigned_repr.
      rep_omega.
    }
    rewrite H2.
    Search (Int.signed (Int.repr _)).
    apply Int.signed_repr.
    rep_omega.
  }
  {
    split3; simpl; auto.
    rewrite Z.max_r; rep_omega.
  }
  {
    Intros p.
    forward_if
      (PROP ( )
       LOCAL (temp _p p; temp _s s;gvars gv)
       SEP (mem_mgr gv;
            malloc_token Ews (tarray tschar (Zlength sigma + 1)) p;
            cstring sh sigma s;
            data_at_ Ews (tarray tschar (Zlength sigma + 1)) p)).
    {
      change (Memory.EqDec_val p nullval) with (eq_dec p nullval).
      if_tac; entailer!.
    }
    {
      forward_call tt. (* Call to exit *)
      contradiction.
    }
    {
      rewrite if_false by auto.
      Intros.
      forward.  (*  /*skip*/;  *)
      entailer.
      unfold cstring.
      entailer!.
    }
    (* Call to strcpy *)
    forward_call (Ews, sh, p, (Zlength sigma + 1), s, sigma).
    split3; auto.
    omega.
    forward.
    Exists p.
    entailer!.
    unfold cstringn, cstring.
    entailer!.
    assert((Zlength sigma + 1 - (Zlength sigma + 1)) = 0). {
      rep_omega.
    }
    rewrite H7.
    assert(Z.to_nat 0 = 0%nat). {
      reflexivity.
    }
    rewrite H8.
    simpl.
    entailer!.
    rewrite app_nil_r.
    entailer!.
  }
Qed.

Lemma body_new_cell: semax_body Vprog Gprog f_new_cell new_cell_spec.
Proof.
  start_function.
  forward_call (tcell, gv). (* malloc *)
  {
    (* preconditions of malloc *)
    split3; simpl; try rep_omega;auto.
  }
  Intros p.
  forward_if
    (PROP ( )
     LOCAL (temp _p p; temp _key s; temp _count (Vint (Int.repr count)); temp _next next; gvars gv)
     SEP (mem_mgr gv;
          malloc_token Ews tcell p;
          data_at_ Ews tcell p;
          cstring sh key s)).
  { (* *)
    destruct  (Memory.EqDec_val p nullval);
    if_tac;entailer!.
  }
  {
    forward_call tt.
    inversion H1.
  }
  {
    rewrite if_false by auto.
    forward.
    entailer!.
  }
  { (* Call copy_string *)
    forward_call (sh, s, key, gv).
    Intros key_copy.
    forward.
    forward.
    forward.
    forward.
    Exists p.
    unfold list_cell.
    Exists key_copy.
    entailer!.
  }

Qed.

(** [] *)

(* ================================================================= *)
(** ** Auxiliary lemmas about data-structure predicates *)


(** **** Exercise: 2 stars (iter_sepcon_hints)  *)
Lemma iter_sepcon_listrep_local_facts:
 forall bl, iter_sepcon (uncurry listrep) bl
                    |-- !! Forall is_pointer_or_null (map snd bl).
Proof.
(* Hint: use [induction] and [sep_apply]. *)
  intros.
  induction bl as [| h t].
  { (* bl is nil *)
    simpl.
    entailer!.
  }
  { (* bl is h :: t *)
    destruct h as [hc hp].
    simpl.
    sep_apply IHt.
    unfold uncurry at 2.
    simpl.
    entailer!.
  }
Qed.


Hint Resolve iter_sepcon_listrep_local_facts : saturate_local.
(** [] *)

(** **** Exercise: 2 stars (iter_sepcon_split3)  *)
Lemma iter_sepcon_split3:
  forall {A}{d: Inhabitant A} (i: Z) (al: list A) (f: A -> mpred),
   0 <= i < Zlength al   ->
  iter_sepcon f al =
  iter_sepcon f (sublist 0 i al) * f (Znth i al) * iter_sepcon f (sublist (i+1) (Zlength al) al).
Proof.
intros.
rewrite <- (sublist_same 0 (Zlength al) al) at 1 by auto.
(* Hint: [rewrite (sublist_split LO MID HI) by omega], where you choose values for LO MID HI.
  Also useful:  [rewrite sublist_len_1]    and    [iter_sepcon_app].
*)
rewrite (sublist_split 0 i (Zlength al)) by omega.
rewrite (sublist_split i (i+1) (Zlength al)) by omega.
rewrite (sublist_len_1 i); auto.
rewrite iter_sepcon_app.
rewrite iter_sepcon_app.
simpl.
remember (iter_sepcon f (sublist 0 i al)) as P1.
remember (iter_sepcon f (sublist (i + 1) (Zlength al) al)) as P3.
rewrite sepcon_emp.
rewrite sepcon_assoc.
reflexivity.
Qed.

(** [] *)

(** **** Exercise: 3 stars (body_new_table)  *)
Lemma body_new_table_helper:
 (* This lemma is useful as the very last thing to do in body_new_table *)
 forall p,
   data_at
     Ews thashtable (list_repeat (Z.to_nat N) nullval) p
  |-- field_at Ews thashtable [StructField _buckets]
       (list_repeat (Z.to_nat N) nullval) p *
         iter_sepcon (uncurry listrep) (list_repeat (Z.to_nat N) ([], nullval)).
Proof.
intros.
unfold_data_at 1%nat.
entailer!.
assert(forall n: nat, iter_sepcon (uncurry listrep) (list_repeat n ([], nullval)) = emp). {
  intro n.
  induction n as [|n'].
  { (* n is 0 *)
    simpl; auto.
  }
  { (* n is S n' *)
    assert((forall nn: nat, S nn = 1 + nn)%nat). {
      intros;reflexivity.
    }
    rewrite H1.
    rewrite <- list_repeat_app.
    simpl.
    rewrite IHn'.
    rewrite sepcon_emp.
    unfold uncurry.
    simpl.
    autorewrite with norm.
    auto.
  }
}
rewrite (H1 (Z.to_nat N)).
entailer.
Qed.

Lemma upd_single_repeat:
  forall (A: Type) (old new: A),
    upd_Znth 0 (list_repeat (Z.to_nat 1) old) new =
    (list_repeat (Z.to_nat 1) new).
Proof.
  intros.
  simpl.
  unfold upd_Znth.
  simpl.
  unfold Zlength.
  simpl.
  rewrite sublist_nil.
  reflexivity.
Qed.


Lemma body_new_table: semax_body Vprog Gprog f_new_table new_table_spec.
Proof.
(** The loop invariant in this function describes a partially initialized array.
   The best way to do that is with something like,

  data_at Tsh thashtable
      (list_repeat (Z.to_nat i) nullval ++
       list_repeat (Z.to_nat (N-i)) Vundef)   p.

  Then at some point you'll have to prove something about,

   data_at Tsh thashtable
      (list_repeat (Z.to_nat (i + 1)) nullval ++
       list_repeat (Z.to_nat (N - (i + 1))) Vundef)   p

  In particular, you'll have to split up [list_repeat (Z.to_nat (i + 1)) nullval]
   into [list_repeat (Z.to_nat i) nullval ++ list_repeat (Z.to_nat 1) nullval].
  The best way to do that is [rewrite <- list_repeat_app'.]
*)
  start_function.
  forward_call (thashtable, gv).
  { (* preconditions of malloc *)
    split3; simpl; try rep_omega;auto.
  }
  Intros p.
  forward_if
    (PROP (p <> nullval)
     LOCAL (temp _p p; gvars gv)
     SEP (mem_mgr gv;
          malloc_token Ews thashtable p;
          data_at_ Ews thashtable p)).
  { (* *)
    destruct  (Memory.EqDec_val p nullval).
    * entailer!.
    * rewrite if_false by auto.
      entailer!.
  }
  {
    forward_call tt.
    inversion H0.
  }
  forward.
  entailer!.
  rewrite if_false by auto.
  entailer!.
  forward_for_simple_bound
    109
    (EX i:Z,
          (PROP (p <> nullval)
           LOCAL (temp _p p; gvars gv)
           SEP (mem_mgr gv; malloc_token Ews thashtable p *
                data_at Ews thashtable
                        (list_repeat (Z.to_nat i) nullval ++
                                     list_repeat (Z.to_nat (N-i)) Vundef)
                        p)))%assert.
  { (* Proof pre-state implies the invariant *)
    entailer!.
    simpl.
    unfold data_at_, data_at, field_at_.
    entailer!.
  }
  {
    Intros.
    forward.
    rewrite Z2Nat.inj_add by omega.
    rewrite <- list_repeat_app.
    assert((N-i) = 1 + (N - (i+1))). {
      omega.
    }
    rewrite H1.
    rewrite Z2Nat.inj_add; try omega.
    rewrite <- list_repeat_app.
    rewrite upd_Znth_app2.
    {
      rewrite Zlength_list_repeat by omega.
      assert(i - i = 0). {
        omega.
      }
      rewrite H2.
      rewrite upd_Znth_app1.
      {
        remember (list_repeat (Z.to_nat i) nullval) as part1.
        remember (list_repeat (Z.to_nat (N - (i + 1))) Vundef) as part3.
        rewrite upd_single_repeat.
        assert(Int.unsigned (Int.repr 0) = 0). {
          rewrite Int.unsigned_repr; [reflexivity | rep_omega].
        }
        rewrite H3.
        entailer!.
        assert(Vlong (Int64.repr 0) = nullval). {
          unfold nullval.
          unfold Archi.ptr64.
          reflexivity.
        }
        rewrite H7.
        rewrite app_assoc.
        entailer!.
      }
      {
        rewrite Zlength_list_repeat; omega.
      }
    }
    {
      rewrite Zlength_list_repeat by omega.
      rewrite Zlength_app.
      rewrite Zlength_list_repeat by omega.
      rewrite Zlength_list_repeat.
      {
        rewrite <- H1.
        rewrite N_eq in *.
        omega.
      }
      {
        rewrite N_eq in *.
        omega.
      }
    }
    rewrite N_eq in *.
    omega.
  }
  { (* At return. *)
    forward.
    rewrite <- N_eq in *.
    rewrite Z.sub_diag in *.
    simpl in *.
    rewrite <- app_nil_end in *.
    Exists p.
    entailer!.
    unfold hashtable_rep.
    Exists (list_repeat (Z.to_nat N) ((@nil (string * Z)), nullval)).
    entailer!.
    assert((map snd (list_repeat (Z.to_nat N) ((@nil (string * Z), nullval)))) =
           (list_repeat (Z.to_nat N) nullval)). {
      rewrite map_list_repeat.
      reflexivity.
    }
    rewrite H4.
    apply (body_new_table_helper p).
  }

Qed.

(** [] *)

(** **** Exercise: 3 stars (body_get)  *)
(** The [get] function traverses a linked list.  We will reason about linked-list traversal
   in separation logic using "Magic Wand as Frame" http://www.cs.princeton.edu/~appel/papers/wand-frame.pdf
   When the loop is partway down the linked list, we can view the original list
   up to the current position as a "linked-list data structure with a hole";
   and the current position points to a linked-list data structure that fills the hole.
   The "data-structure-with-a-hole" we reason about with separating implication,
    called "magic wand":   (hole -* data-structure)
    which says, if you can conjoin this data-structure-with-a-hole
    with something-to-fill-the-hole, then you get the original data structure:
     hole * (hole -* data-structure) |--   data-structure.
    The lemma [listrep_traverse] is useful in moving one linked-list cell
    out of the hole and into the data-structure-with-a-(smaller)-hole.
*)

Lemma listrep_traverse:  (* useful in body_get lemma*)
  forall key count p' p,
  list_cell key count p' p |--
  ALL al:list (string * Z), listrep al p' -* (EX y : val, list_cell key count y p * listrep al y).
Proof.
intros.
apply allp_right. intro al.
apply wand_sepcon_adjoint.
Exists p'.
entailer!.
Qed.

Lemma is_pointer_or_null_expand:
  forall p: val, is_pointer_or_null p -> isptr p \/ p = nullval.
Proof.
  intros.
  destruct p;simpl in *; try inversion H.
  { (* Vlong *)
    right; auto.
  }
  { (* Vptr *)
    left;auto.
  }
Qed.

Lemma isptr_expand:
  forall p: val, isptr p -> exists (b : block) (o: ptrofs), p = Vptr b o.
Proof.
  intros.
  destruct p eqn: ?; try (simpl in H; inversion H).
  exists b.
  exists i.
  reflexivity.
Qed.

Lemma wand_cancel:
  forall P Q R,
    P * (P * Q -* R) |-- Q -* R.
Proof.
  intros.
  rewrite <- wand_sepcon_adjoint.
  rewrite sepcon_assoc.
  rewrite (sepcon_comm (P * Q -* R) Q).
  rewrite <- sepcon_assoc.
  apply modus_ponens_wand.
Qed.

Lemma wand_cancel':
  forall P Q R,
    Q * P * (P * Q -* R) |-- Q * (Q -* R).
Proof.
  intros.
  sep_apply (wand_cancel P Q R).
  rewrite sepcon_comm.
  auto.
Qed.

Lemma emp_wand:
  forall P,
    (emp -* P) = P.
Proof.
  intros.
  apply pred_ext.
  { (* emp -* P |-- P *)
    rewrite <- (sepcon_emp (emp -* P)).
    rewrite sepcon_comm.
    apply modus_ponens_wand.
  }
  { (* P |-- emp -* P *)
    apply wand_sepcon_adjoint.
    rewrite sepcon_emp.
    auto.
  }
Qed.

Lemma exp_eq:
  forall (A: Type) (P: A -> mpred),
    (EX y: A, P y) = (EX y':A, P y').
Proof.
  auto.
Qed.

Lemma sepcon_exp:
  forall (A: Type) (y: A) (P Q: A -> mpred),
    (P y) * (Q y) |--  (EX y':A, (P y') * (Q y')).
Proof.
  intros.
  apply (exp_right y).
  auto.
Qed.

Lemma sepcon_exp':
  forall (A: Type) (y: A) (P: A -> mpred),
    (P y) |--  (EX y':A, (P y')).
Proof.
  intros.
  apply (exp_right y).
  auto.
Qed.

(*
Lemma wand_ex_intro:
  forall (A: Type) (y: A) (P: A -> mpred) (Q: mpred),
    (EX y':A, (P y')) -* Q |--  ((P y) -* Q).
Proof.
  intros.
  remember (EX y' : A, P y') as x'.
  rewrite <- (wand_sepcon_adjoint
Qed.
*)
Lemma wand_cancel_ex:
  forall (A: Type) (y: A) (P Q: A -> mpred) (R: mpred),
    (P y) * ((EX y':A, ((P y') * (Q y'))) -* R) |-- (Q y) -* R.
Proof.
  intros.
  rewrite <- wand_sepcon_adjoint.
  sep_apply (sepcon_exp A y P Q).
  apply wand_frame_elim.
Qed.

Lemma wand_cancel_ex':
  forall (A: Type) (y: A) (P Q: A -> mpred) (R: mpred),
    (Q y) * (P y) * ((EX y':A, ((P y') * (Q y'))) -* R) |-- R.
Proof.
  intros.
  rewrite sepcon_assoc.
  rewrite sepcon_comm.
  rewrite wand_sepcon_adjoint.
  sep_apply (wand_cancel_ex A y P Q R).
  auto.
Qed.

Lemma body_get: semax_body Vprog Gprog f_get get_spec.
Proof.
  start_function.
  rename p into table.
  pose proof (hashfun_inrange sigma).
  forward_call (sh, s, sigma). (* h = hash(s) *)
  forward.
  rewrite modu_repr; [| auto |  rep_omega].
  remember ((hashfun sigma) mod 109) as bk.
  assert(0 <= bk < 109). {
    rewrite Heqbk.
    apply (Z.mod_pos_bound (hashfun sigma) 109); omega.
  }
  unfold hashtable_rep.
  Intros bl.
  remember (map snd bl) as buckets.
  forward.
  {
    
    entailer.
  }
  {
    entailer!.
    simplify_value_fits in H3.
    destruct H3.
    autorewrite with sublist in *|-.
    remember ((hashfun sigma) mod 109) as bk.
    assert(0 <= bk < Zlength bl). {
      rewrite H0.
      rewrite Heqbk.
      apply (Z.mod_pos_bound (hashfun sigma) 109); omega.
    }
    rewrite Znth_map by auto.
    rewrite Forall_map in H4.
    apply Forall_Znth; auto.
  }
{
  rewrite <- Heqbk.
  autorewrite with norm.
  deadvars!.
  assert_PROP(Zlength bl = 109). {
    entailer!.
    simplify_value_fits in H4.
    destruct H4.
    autorewrite with sublist in *|-.
    auto.
  }
  assert(0 <= bk < Zlength bl). {
    rewrite H1.
    rewrite Heqbk.
    apply (Z.mod_pos_bound (hashfun sigma) 109); omega.
  }
  rewrite (iter_sepcon_split3 bk bl (uncurry listrep) H2).

  unfold uncurry at 2.
  remember (Znth bk buckets) as p0.
  remember (Znth bk contents) as l0.
  rewrite <- (Znth_map bk fst bl) by auto.
  rewrite <- (Znth_map bk snd bl) by auto.
  rewrite <- Heqbuckets.
  rewrite <- H0.
  rewrite H1.
  (* So p0 is pointing to l0 the list we now traverse. *)
  forward_while (EX p: val, EX l: list (string * Z),
     PROP (l0 = (Znth bk contents);
           p0 = (Znth bk buckets);
           list_get sigma l = list_get sigma l0)
     LOCAL (temp _p p;
            temp _s s)
     SEP (cstring Tsh sigma s;
          malloc_token Tsh thashtable table;
          field_at Tsh thashtable [StructField _buckets] (map snd bl) table;
          iter_sepcon (uncurry listrep) (sublist 0 bk bl);
          listrep l p;
          listrep l p -* listrep l0 p0;
          iter_sepcon (uncurry listrep) (sublist (bk + 1) (Zlength bl) bl)))%assert.
  { (* subgoal 1 *)
    Exists p0 l0.
    entailer!.
    simplify_value_fits in H5.
    destruct H5.
    remember (listrep (Znth (hashfun sigma mod 109) (map fst bl)) (Znth (hashfun sigma mod 109) (map snd bl))) as P.
    cancel.
    autorewrite with sublist in *|-.
    rewrite H0.
    remember (iter_sepcon (uncurry listrep) (sublist (hashfun sigma mod 109 + 1) 109 bl)) as Q.
    cancel.
    apply wand_refl_cancel_right.
  }
  { (* subgoal 2 *)
    entailer!.
  }
  { (* subgoal 3 *)
    destruct l as [| lh lt].
    { (* l is nil *)
      unfold listrep at 2.
      normalize.
    }
    { (* l is lh :: lt *)
      unfold listrep at 2.
      fold listrep.
      unfold list_cell.
      destruct lh as [key count].
      Intros y kp.
      forward.
      forward_call (kp, key, s, sigma).
      Intros strcmp_ret.
      forward_if.
      { (* True branch *)
        rewrite pred_dec_true in H6 by auto.
        simpl in H5.
        rewrite pred_dec_true in H5 by auto.
        forward.
        sep_apply (listcell_fold key kp count y p).
        sep_apply (listrep_fold key count y p lt).
        sep_apply (wand_frame_elim (listrep ((key, count) :: lt) p) (listrep l0 p0)).
        assert((listrep l0 p0) = (uncurry listrep) (l0, p0)). {
          unfold uncurry.
          reflexivity.
        }
        rewrite H8.
        assert((l0, p0) = Znth bk bl). {
          subst l0 p0.
          subst contents buckets.
          rewrite Znth_map by auto.
          rewrite Znth_map by auto.
          remember (Znth bk bl) as bkbl.
          rewrite <- surjective_pairing; reflexivity.
        }
        rewrite H9.
        assert((iter_sepcon (uncurry listrep) (sublist 0 bk bl) *
                uncurry listrep (Znth bk bl) *
                iter_sepcon (uncurry listrep) (sublist (bk + 1) (Zlength bl) bl))
                 |--
                 (iter_sepcon (uncurry listrep) bl)). {
          apply derives_refl''.
          apply iter_sepcon_split3.
          auto.
        }
        sep_apply H10.
        forward.
        entailer!.
        {
          unfold hashtable_get.
          rewrite Zlength_map.
          rewrite H1.
          reflexivity.
        }
        {
          unfold hashtable_rep.
          Exists bl.
          entailer!.
        }
      }
      (* After the if. Apparently the empty else clause does have any requirements. *)
      forward. (* Over p=p->next; *)

      (* At end of loop. Show the invariant still holds. *)
      rewrite if_false in H6 by auto.
      Exists (y, lt).
      simpl in H5.
      rewrite if_false in H5.
      2: auto.
      sep_apply (listcell_fold key kp count y p).
      simpl (listrep ((key, count) :: lt) p).
      assert (list_cell key count y p *
              ((EX y' : val, list_cell key count y' p * listrep lt y') -* listrep l0 p0)
                |-- listrep lt y -* listrep l0 p0).
      {
        exact (wand_cancel_ex val y
                                (fun y': val => list_cell key count y' p)
                                (fun y': val => listrep lt y')
                                (listrep l0 p0)).
      }
      sep_apply H8.
      simpl (listrep (snd (y, lt)) (fst (y, lt))).
      entailer!.
    }
  }
  {
    assert_PROP(l = nil). {
      destruct l as [| [lhs lhx] lt].
      { (* l is nil *)
        entailer!.
      }
      { (* l id lh :: lt *)
        unfold listrep at 2.
        Intros y.
        fold (listrep lt y).
        assert_PROP(isptr p). {
          entailer!.
      }
        rewrite HRE in H6.
        simpl in H6.
        contradiction.
      }
    }
    rewrite H6 in H5.
    simpl in H5.
    forward. (* Over return *)
    entailer!.
    {
      unfold hashtable_get.
      rewrite Zlength_map.
      rewrite H1.
      rewrite <- H5.
      reflexivity.
    }
    unfold hashtable_rep.
    Exists bl.
    entailer!.
    remember (hashfun sigma mod 109) as bk.
    remember (listrep [] nullval) as nl.
    remember (listrep (Znth bk (map fst bl)) (Znth bk (map snd bl))) as Q.
    sep_apply (modus_ponens_wand nl Q).
    subst nl Q.
    rewrite (iter_sepcon_split3 bk bl (uncurry listrep)) by auto.
    cancel.
    unfold uncurry.
    repeat rewrite Znth_map by auto.
    auto.
  }

}

Qed.

(** [] *)

(** **** Exercise: 3 stars (listboxrep_traverse)  *)
Lemma listboxrep_traverse:
  forall p kp key count r,
          field_compatible tcell [] p ->
            cstring Tsh key kp *
            field_at Tsh tcell [StructField _key] kp p *
            field_at Tsh tcell [StructField _count] (Vint (Int.repr count)) p *
            malloc_token Tsh tcell p *
            data_at Tsh (tptr tcell) p r |--
            ALL dl: list (string * Z),
              listboxrep dl (field_address tcell [StructField _next] p)
                -* listboxrep ((key, count) :: dl) r.
Proof.
  intros.
  apply allp_right; intro dl.
  apply -> wand_sepcon_adjoint.
   (** Sometime during the proof below, you will have
       [data_at Tsh tcell ... p]
     that you want to expand into

       field_at Tsh tcell [StructField _key] ... p
     * field_at Tsh tcell [StructField _count] ... p
     * field_at Tsh tcell [StructField _next] ... p].

   You can do this with   [unfold_data_at x%nat] where x is the number
   indicating _which_ of the [data_at] or [field_at] conjucts you want to expand.
*)

  unfold listboxrep.
  Intros p'.
  remember (field_address tcell [StructField _next] p) as y.
  Exists p.
  simpl (listrep ((key, count) :: dl) p).
  Exists p'.
  unfold list_cell.
  Exists kp.
  unfold_data_at 4%nat.
  entailer!.
  rewrite field_at_data_at.
  entailer!.
Qed.

(** [] *)

(** **** Exercise: 2 stars (body_incr_list)  *)

Lemma incr_list_no_key:
  forall (key: string) (al: list (string * Z)),
    ~ (In key (map fst al)) -> (list_incr key al) = al ++ [(key, 1)].
Proof.
  intros key al.
  induction al as [| [lhk lhc] lt]; intros.
  { (* al is nil *)
    reflexivity.
  }
  { (* al is (lhk, lhc) :: lt *)
    simpl.
    simpl in H.
    destruct (EqDec_string key lhk).
    { (* key = lhk *)
      exfalso.
      apply H.
      left.
      auto.
    }
    { (* key <> lhk *)
      f_equal.
      apply IHlt.
      intro.
      apply H.
      right.
      auto.
    }
  }
Qed.

Lemma incr_list_with_key:
  forall (key: string) (al post: list (string * Z)) (count: Z),
    ~ (In key (map fst al)) ->
    (list_incr key (al ++ [(key, count)] ++ post)) = (al ++ [(key, count + 1)] ++ post).
Proof.
  intros key al.
  induction al as [| [lhk lhc] lt]; intros.
  { (* al is nil *)
    simpl.
    rewrite pred_dec_true by auto.
    reflexivity.
  }
  { (* al is (lhk, lhc) :: lt *)
    simpl.
    simpl in H.
    destruct (EqDec_string key lhk).
    { (* key = lhk *)
      exfalso.
      apply H.
      left.
      auto.
    }
    { (* key <> lhk *)
      f_equal.
      apply IHlt.
      intro.
      apply H.
      right.
      auto.
    }
  }

Qed.

Definition partial_listbox_rep(P: (list (string * Z)) -> (list (string * Z)))
           (r i: val) : mpred :=
  ALL l: list (string * Z), ((listboxrep l i) -* (listboxrep (P l) r)).

Lemma ptbr3a:
  forall q p kp key count,
    ((data_at Tsh (tptr tcell) p q) *
     field_at Tsh tcell [StructField _key] kp p *
     field_at Tsh tcell [StructField _count] (Vint (Int.repr count)) p *
     cstring Tsh key kp *
     malloc_token Tsh tcell p
    )
      |-- (partial_listbox_rep (fun l => (key, count) :: l) q
                               (field_address tcell [StructField _next] p)).
Proof.
  intros.
  unfold partial_listbox_rep.
  apply allp_right.
  intro v.
  rewrite <- wand_sepcon_adjoint.
  unfold listboxrep at 2.
  Exists p.
  unfold listrep.
  unfold listboxrep.
  Intros y.
  Exists y.
  fold (listrep v y).
  unfold list_cell.
  Exists kp.
  cancel.
  unfold_data_at 2%nat.
  cancel.
  rewrite field_at_data_at.
  simpl.
  unfold tcell.
  auto.
Qed.

Lemma ptbr3c:
  forall p,
    emp |-- partial_listbox_rep (fun l => l) p p.
Proof.
  intros.
  unfold partial_listbox_rep.
  apply allp_right.
  intro v.
  apply wand_refl_cancel_right.
Qed.

Lemma ptbr3d:
  forall t i r P,
  ((listboxrep t i) * (partial_listbox_rep P r i))
    |-- (listboxrep (P t) r).
Proof.
  intros.
  unfold partial_listbox_rep.
  rewrite sepcon_comm.
  apply wand_sepcon_adjoint.
  apply (allp_instantiate
           (fun l : list (string * Z) => listboxrep l i -* listboxrep (P l) r)
           t).
Qed.

Lemma ptbr3e:
  forall P1 P2 p1 p2 p3,
    (partial_listbox_rep P1 p2 p1) *
    (partial_listbox_rep P2 p3 p2)
      |-- (partial_listbox_rep
             (fun l: list (string * Z) => (P2 (P1 l)))
             p3 p1).
Proof.
  intros.
  unfold partial_listbox_rep.
  apply allp_right.
  intro v.
  assert((ALL l : list (string * Z) , listboxrep l p1 -* listboxrep (P1 l) p2)
           |-- (listboxrep v p1 -* listboxrep (P1 v) p2)). {
    apply (allp_instantiate
             (fun l : list (string * Z) => listboxrep l p1 -* listboxrep (P1 l) p2)
             v).
  }
  sep_apply H; clear H.
  assert((ALL l : list (string * Z) , listboxrep l p2 -* listboxrep (P2 l) p3)
           |-- listboxrep (P1 v) p2 -* listboxrep (P2 (P1 v)) p3). {
    apply (allp_instantiate
             (fun l : list (string * Z) => listboxrep l p2 -* listboxrep (P2 l) p3)
             (P1 v)).
  }
  sep_apply H; clear H.
  rewrite <- wand_sepcon_adjoint.
  sep_apply (modus_ponens_wand (listboxrep v p1) (listboxrep (P1 v) p2)).
  sep_apply (modus_ponens_wand (listboxrep (P1 v) p2) (listboxrep (P2 (P1 v)) p3)).
  auto.
Qed.

Lemma listboxrep_fold:
  forall p r al,
    data_at Tsh (tptr tcell) p r * listrep al p |-- listboxrep al r.
Proof.
  intros.
  unfold listboxrep.
  Exists p.
  cancel.
Qed.

Lemma hashtable_rep_fold: forall (p: val) (bl: list (list (string * Z) * val)),
    malloc_token Tsh thashtable p *
    field_at Tsh thashtable [StructField _buckets] (map snd bl) p *
    iter_sepcon (uncurry listrep) bl
                |-- hashtable_rep (map fst bl) p.
Proof.
  intros.
  unfold hashtable_rep.
  Exists bl.
  entailer!.
Qed.

Lemma field_at_data_at':
      forall {cs: compspecs} (sh : Share.t) (t : type) (gfs : list gfield)
         (v : reptype (nested_field_type t gfs))
         (p : val),
       field_at sh t gfs v p =
       !! field_compatible t gfs p  && data_at sh (nested_field_type t gfs) v (field_address t gfs p).
Proof.
intros.
apply pred_ext.
entailer!. rewrite field_at_data_at; auto.
entailer!. rewrite field_at_data_at; auto.
Qed.

Lemma add1_rep:
  forall x: Z,
    (Int.add (Int.repr x) (Int.repr 1)) = Int.repr (x + 1).
Proof.
  intros.
  unfold Int.add.
  Search ((_ + _) mod _).
  apply int_rep_helper.
  rewrite !Int.unsigned_repr_eq.
  remember Int.modulus as m.
  rewrite <- Zplus_mod.
  auto.
Qed.

Definition prepend_key_count (P: (list (string * Z)) -> (list (string * Z)))
           (key: string) (count : Z) (l: list (string * Z)) :=
    P((key, count) :: l).

Lemma list_rep_listbox_rep: forall (t: list (string * Z)) (p : val),
    listrep t p =
    match t with
    | [] => !!(p = nullval) && emp
    | (s, c) :: tl => (EX kp: val,
      malloc_token Tsh tcell p *
      cstring Tsh s kp *
      field_at Tsh tcell [StructField _key] kp p *
      field_at Tsh tcell [StructField _count] (Vint (Int.repr c)) p *
      listboxrep tl (field_address tcell [StructField _next] p))
    end.
Proof.
  intros.
  destruct t as [| [s c] tl]; auto.
  unfold listboxrep; simpl.
  apply pred_ext.
  {
    Intros y.
    unfold list_cell.
    Intros kp.
    Exists kp.
    entailer!.
    unfold_data_at 1%nat.
    Exists y.
    cancel.
    rewrite !field_at_data_at.
    cancel.
  }
  {
    Intros kp p0.
    Exists p0.
    unfold list_cell.
    Exists kp.
    entailer!.
    unfold_data_at 2%nat.
    rewrite !field_at_data_at.
    cancel.
  }
Qed.

Lemma listboxrep_spec: forall (t: list (string * Z)) (b: val),
  listboxrep t b =
  EX p: val,
  data_at Tsh (tptr tcell) p b *
    match t with
    | [] => !!(p = nullval) && emp
    | (s, c) :: tl => (EX kp: val,
      malloc_token Tsh tcell p *
      cstring Tsh s kp *
      field_at Tsh tcell [StructField _key] kp p *
      field_at Tsh tcell [StructField _count] (Vint (Int.repr c)) p *
      listboxrep tl (field_address tcell [StructField _next] p))
    end.
Proof.
  intros.
  unfold listboxrep at 1.
  f_equal.
  extensionality p.
  destruct t as [| [s c] tl]; simpl.
  { apply pred_ext; entailer!. }
  { (* Non-nil case *)
    apply pred_ext; entailer!.
    {
      unfold list_cell.
      Intros kp. Exists kp.
      unfold_data_at 2%nat.
      cancel.
      unfold listboxrep.
      Exists y.
      cancel.
      rewrite field_at_data_at.
      cancel.
    }
    {
      unfold listboxrep.
      Intros q.
      Exists q.
      cancel.
      unfold list_cell.
      Exists kp.
      cancel.
      unfold_data_at 2%nat.
      cancel.
      rewrite field_at_data_at.
      cancel.
    }
  }
Qed.

Lemma listboxrep_internal: forall b p kp s c tl,
  data_at Tsh (tptr tcell) p b *
  malloc_token Tsh tcell p *
  cstring Tsh s kp *
  field_at Tsh tcell [StructField _key] kp p *
  field_at Tsh tcell [StructField _count] (Vint (Int.repr c)) p *
  listboxrep tl (field_address tcell [StructField _next] p) |--
  listboxrep ((s, c) :: tl) b.
Proof.
  intros.
  rewrite (listboxrep_spec (cons _ _ )).
  Exists p kp.
  entailer!.
Qed.

Lemma body_incr_list: semax_body Vprog Gprog f_incr_list incr_list_spec.
Proof.
(** This proof uses "magic wand as frame" to traverse _and update_
   a (linked list) data structure.   This pattern is a bit more complex than
   the wand-as-frame pattern used in body_get, which did not update
   the data structure.   You will still use "data-structure-with-a-hole"
   and "what-is-in-the-hole"; but now the "data-structure-with-a-hole"
   must be able to accept the _future_ hole-filler, not the one that is
   in the hole right now.

  The key lemmas to use are, [wand_refl_cancel_right], [wand_frame_elim'],
   and [wand_frame_ver].   When using [wand_frame_ver], you will find
   [listboxrep_traverse] to be useful.
*)
  start_function.
  forward.
  rename al into al0.
  forward_loop
    (EX P: (list (string * Z)) -> (list (string * Z)),
           (EX r: val, (EX al: list (string * Z),
       (PROP (
            P(list_incr sigma al) = list_incr sigma al0;
            (list_get sigma al0) = (list_get sigma al)
          )
        LOCAL (
          temp _r r;
          temp _s s
        )
        SEP (
          listboxrep al r;
          partial_listbox_rep P r0 r;
          cstring Tsh sigma s
        )
    )))).
  { (* Pre implies loop invariant *)
    Exists (fun l: (list (string * Z)) => l).
    Exists r0.
    Exists al0.
    entailer!.
    apply ptbr3c.
  }
  { (* In body of loop *)
    Intros P r al.
    unfold listboxrep.
    Intros p.
    forward.
    forward_if (p <> nullval).
    { (* True branch *)
      assert_PROP(al = []). {
        entailer!.
        apply H5;reflexivity.
      }
      subst al.
      simpl in H0.
      forward_call (s, sigma, 1, nullval).
      Intros new_cell_ret.
      forward.
      sep_apply (listrep_fold_nil sigma 1 new_cell_ret).
      sep_apply (listboxrep_fold new_cell_ret r [(sigma, 1)]).
      sep_apply (ptbr3d [(sigma, 1)] r r0 P).
      rewrite H0.
      forward.
    }
    { (* False branch *)
      forward.
      entailer!.
    }
    (* After first if. *)
    assert_PROP(p <> nullval). {
      entailer!.
    }
    destruct al as [| [key' count'] lt]; rewrite list_rep_listbox_rep.
    { (* al is nil, a contradiction *)
      normalize.
    }
    { (* al is (key', count') :: lt *)
      Intros kp. clear H2.
      forward. (* temp = p-> key *)
      forward_call (kp, key', s, sigma).
      Intros strcmp_ret.
      forward_if.
      { (* True branch *)
        subst strcmp_ret.
        rewrite pred_dec_true in H2 by auto.
        subst key'.
        simpl in H0.
        rewrite pred_dec_true in H0 by auto.
        simpl in H1.
        rewrite pred_dec_true in H1 by auto.
        rewrite H1 in H. clear H1.
        forward.
        forward.
        rewrite (add1_rep count').
        sep_apply (listboxrep_internal r p kp sigma (count' + 1) lt).
        forward.
        cancel.
        rewrite <- H0.
        apply ptbr3d.
      }
      { (* False branch, key did not compare *)
        forward.
        rewrite pred_dec_false in H2 by auto.
        simpl in H0.
        rewrite pred_dec_false in H0 by auto.
        Exists (fun l => P((key', count') :: l)).
        Exists (field_address tcell [StructField _next] p).
        Exists lt.
        entailer!.
        {
          simpl in H1.
          rewrite pred_dec_false in H1 by auto.
          auto.
        }
        {
          sep_apply (ptbr3a r p kp key' count').
          apply ptbr3e.
        }
      }
    }
  }

Qed.

(** [] *)



(** Examine this carefully: *)
Check wand_slice_array.
(*  : forall (lo hi n : Z) (t : type) (sh : Share.t)
             (al' : list (reptype t)) (p : val),
       0 <= lo <= hi ->
       hi <= n ->
       Zlength al' = n ->
       data_at sh (tarray t n) al' p =
       !! field_compatible (tarray (tptr tcell) n) [] p &&
       data_at sh (tarray t (hi - lo))
         (sublist lo hi al')
         (field_address0 (tarray t n) [ArraySubsc lo] p) *
       array_with_hole sh t lo hi n al p.
*)
(** Here (array_with_hole sh t lo hi n al p) means *)
(*  :  (ALL cl : list (reptype t) ,
         data_at sh (tarray t (hi - lo)) cl
           (field_address0 (tarray t n) [ArraySubsc lo] p) -*
         data_at sh (tarray t n)
           (sublist 0 lo al' ++ cl ++ sublist hi n al') p)
*)

Lemma wand_slice_array_fold:
  forall {cs: compspecs} lo hi n sh t (al: list (reptype t)) p,
    0 <= lo <= hi ->
    hi <= n ->
    Zlength al = n ->
    (field_compatible (tarray t n) nil p) ->
  data_at sh (tarray t (hi-lo)) (sublist lo hi al) (field_address0 (tarray t n) (ArraySubsc lo :: nil) p) *
  array_with_hole sh t lo hi n al p |--
                  data_at sh (tarray t n) al p.
Proof.
  intros.
  unfold array_with_hole.
  entailer!.
  allp_left (sublist lo hi al).
  rewrite sepcon_comm.
  sep_apply (wand_frame_elim (data_at sh (tarray t (hi - lo)) (sublist lo hi al) (field_address0 (tarray t (Zlength al)) [ArraySubsc lo] p))
                             (data_at sh (tarray t (Zlength al)) (sublist 0 lo al ++ sublist lo hi al ++ sublist hi (Zlength al) al) p)).
  rewrite app_assoc.
  rewrite (sublist_rejoin 0 lo hi al) by omega.
  rewrite (sublist_rejoin 0 hi (Zlength al) al) by omega.
  rewrite sublist_same by auto.
  auto.
Qed.

Lemma wand_slice_array_fold1:
  forall {cs: compspecs} h n sh t (al: list (reptype t)) p,
    0 <= h ->
    (h+1) <= n ->
    Zlength al = n ->
    (field_compatible (tarray t n) nil p) ->
  data_at sh (tarray t ((h+1) - h)) (sublist h (h+1) al) (field_address0 (tarray t n) (ArraySubsc h :: nil) p) *
  array_with_hole sh t h (h+1) n al p |--
                  data_at sh (tarray t n) al p.
Proof.
  intros.
  sep_apply((wand_slice_array_fold h (h+1) n sh t al p)).
  all: auto.
  omega.
Qed.


(** **** Exercise: 4 stars (body_incr)  *)
Lemma body_incr: semax_body Vprog Gprog f_incr incr_spec.
Proof.
start_function.
rename p into table.
rename H into Hmax.
assert_PROP (isptr table) as Htable by entailer!.

(** The next two lines would not be part of an ordinary Verifiable C proof,
   they are here only to guide you through the bigger proof. *)
match goal with
  |- semax _ _ (Ssequence (Ssequence ?c1 (Ssequence ?c2 ?c3)) ?c4) _ =>
  apply (semax_unfold_seq (Ssequence (Ssequence c1 c2) (Ssequence c3 c4))); [ reflexivity | ]
end.
pose (j :=
          EX cts: list (list (string * Z) * val),
                  PROP (contents = map fst cts;
                        0 <= hashfun sigma mod N < N;
                        Zlength cts = N)
                  LOCAL (temp _b (Vint (Int.repr (hashfun sigma mod N)));
                         temp _h (Vint (Int.repr (hashfun sigma)));
                         temp _table table; temp _s s)
                  SEP (cstring Tsh sigma s;
                       malloc_token Tsh thashtable table;
                       data_at Tsh (tarray (tptr tcell) N) (map snd cts) (field_address thashtable [StructField _buckets] table);
                       iter_sepcon (uncurry listrep) cts)).
apply semax_seq' with j;
subst j;
abbreviate_semax. {
  forward_call (s, sigma). (* h = hash(s) *)
  forward.
  unfold hashtable_contents in *.
  unfold hashtable_rep.
  Intros bl.
  Exists bl.
  entailer!.
  {
    split; [split |].
    {
      rewrite N_eq.
      apply Z.mod_pos_bound; omega.
    }
    {
      simplify_value_fits in H2.
      destruct H2.
      rewrite Zlength_map in H.
      rewrite N_eq; auto.
    }
    {
      f_equal.
      unfold Int.modu.
      apply int_rep_helper.
      rewrite !Int.unsigned_repr; try rep_omega.
      Lemma hashfun_limit: forall str,
          0 <= hashfun str <= Int.max_unsigned.
      Proof.
        intros.
        unfold hashfun.
        rewrite hashfun_aux_equiv''.
        unfold hashfun_aux''.
        remember (rev str) as rs.
        destruct rs; simpl.
        {
          rep_omega.
        }
        {
          unfold hash_step'; simpl.
          generalize (fold_right (fun (c : byte) (hin : Z) => hash_step hin c) 0 rs).
          intro z.
          unfold hash_step.
          unfold Int.max_unsigned.
          generalize (z * 65599 + Byte.signed i); intros.
          assert(forall a b: Z, 0 <= a < b ->  0 <= a <= b - 1). {
            intros. omega.
          }
          apply H; clear H.
          apply Z.mod_pos_bound; rep_omega.
        }
      Qed.
      apply (hashfun_limit sigma).
    }
  }
  {
    rewrite field_at_data_at.
    entailer!.
  }
}
{
  Intros cts.
  rewrite H in Hmax.
  assert_PROP(field_compatible thashtable [StructField _buckets] table). {
    entailer!.
  }
  rename H2 into FCT.

  unfold hashtable_get in Hmax.
  rewrite Zlength_map, H1 in Hmax.
  set (h := hashfun sigma mod N) in *.
  remember (map snd cts) as bkptrs.
  remember (field_address thashtable [StructField _buckets] table) as bucketsAdr.
  rewrite (wand_slice_array h (h+1) N Tsh (tptr tcell) bkptrs bucketsAdr);
    try rewrite Heqbkptrs; try rewrite Zlength_map; try rep_omega.

(** For the remainder of the proof, here are some useful lemmas:
    [sublist_len_1] [sublist_same] [sublist_map]
    [data_at_singleton_array_eq]
    [iter_sepcon_split3]  [iter_sepcon_app] [sublist_split]
    [field_at_data_at]
    [wand_slice_array_tptr_tcell]
 *)
  Intros.
  assert(K: h + 1 - h = 1). {
    omega.
  }
  rewrite K. clear K.
  rewrite sublist_len_1; try rewrite Zlength_map; try rewrite H1; auto.
  rewrite <- Heqbkptrs.
  remember (field_address0 (tarray (tptr tcell) N) [ArraySubsc h] bucketsAdr) as bk.
  rewrite (data_at_singleton_array_eq Tsh (tptr tcell) (Znth h bkptrs)).
  rewrite (iter_sepcon_split3 h cts (uncurry listrep));repeat rewrite Zlength_map; repeat rewrite H1; auto.
  unfold uncurry at 2.
  rewrite <- !(Znth_map h fst);repeat rewrite Zlength_map; repeat rewrite H1; auto.
  rewrite <- !(Znth_map h snd);repeat rewrite Zlength_map; repeat rewrite H1; auto.
  rewrite <- Heqbkptrs in *.
  rewrite <- H in *.
  remember (Znth h bkptrs) as p.
  remember (Znth h contents) as al.
  sep_apply (listboxrep_fold p bk al).
  forward_call (bk, al, s, sigma). (* incr_list (& table->buckets[b], s); *)
  3: reflexivity.
  {
    entailer!.
    f_equal.
    assert(FAT: (field_address thashtable [StructField _buckets] table) = table). {
      unfold field_address.
      normalize.
      destruct (field_compatible_dec thashtable [StructField _buckets] table).
      {
        reflexivity.
      }
      {
        contradiction.
      }
    }
    unfold field_address0.
    destruct (field_compatible0_dec (tarray (tptr tcell) N) [ArraySubsc h] (field_address thashtable [StructField _buckets] table)).
    { (* field compatible *)
      simpl.
      normalize.
      rewrite FAT.
      reflexivity.
    }
    unfold field_compatible0 in n.
    exfalso.
    apply n.
    clear n.
    rewrite FAT.
    rewrite FAT in H2.
    destruct H2 as [? A].
    repeat destruct A as [? A].
    repeat split;auto; try omega.
  }
  {
    forward.
    cancel.

    unfold hashtable_rep.
    unfold hashtable_incr.
    rewrite Zlength_map in *.
    rewrite H1 in *.
    unfold h in *.
    clear h.
    remember (hashfun sigma mod N) as h.
    remember (map fst cts) as old_contents.
    remember (list_incr sigma (Znth h old_contents)) as B.
    remember (field_address0 (tarray (tptr tcell) N) [ArraySubsc h] (field_address thashtable [StructField _buckets] table)) as Bp.
    unfold listboxrep.
    Intros p.
    Exists (upd_Znth h cts (B, p)).
    rewrite <- upd_Znth_map.
    simpl.
    rewrite <- Heqold_contents.
    simpl.
    normalize.
    cancel.
    rewrite (iter_sepcon_split3 h (upd_Znth h cts (B, p)) (uncurry listrep)).
    rewrite sublist_upd_Znth_l by omega.
    rewrite sublist_upd_Znth_r; try omega.
    cancel.
    all: rewrite upd_Znth_Zlength.
    all: rewrite H1.
    all: rewrite N_eq in *.
    cancel.
    2-6: omega.
    unfold uncurry.
    remember (map snd cts) as old_bkps.
    repeat rewrite upd_Znth_same by omega.
    simpl.
    rewrite <- (upd_Znth_map snd).
    rewrite <- Heqold_bkps.
    simpl.
    unfold array_with_hole.
    normalize.
    hint.
    autorewrite with sublist.
    rewrite <- HeqBp.
    repeat rewrite upd_Znth_same by omega.
    simpl.
    allp_left [p].
    rewrite sepcon_comm.
    rewrite (data_at_singleton_array_eq Tsh (tptr tcell) p) by auto.
    sep_apply(wand_frame_elim (data_at Tsh (tptr tcell) p Bp)
                              (data_at Tsh (tarray (tptr tcell) 109) (sublist 0 h old_bkps ++ [p] ++ sublist (h + 1) 109 old_bkps)
                                       (field_address thashtable [StructField _buckets] table))).
    assert(Zlength old_bkps = 109). {
      subst old_bkps.
      rewrite Zlength_map; auto.
    }
    assert((sublist 0 h old_bkps ++ [p] ++ sublist (h + 1) 109 old_bkps) =
           (upd_Znth h old_bkps p)). {
      rewrite <- (sublist_same 0 109 old_bkps) at 3 by omega.
      rewrite (sublist_split 0 h 109 old_bkps) by omega.
      rewrite (sublist_split h (h+1) 109 old_bkps) by omega.
      rewrite (upd_Znth_app2 (sublist 0 h old_bkps)).
      f_equal.
      rewrite Zlength_sublist_correct.
      assert(h - (h - 0) = 0). omega.
      rewrite H6; clear H6.
      rewrite upd_Znth0.
      simpl.
      f_equal.
      autorewrite with sublist.
      reflexivity.
      omega.
      omega.
      rewrite Zlength_app.
      repeat rewrite Zlength_sublist_correct by omega.
      omega.
    }
    {
      cancel.
      rewrite field_at_data_at.
      entailer!.
    }
  }
}
Qed.
(** [] *)
