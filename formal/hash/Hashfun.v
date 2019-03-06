(** * Hashfun: Functional model of hash tables *)

(** ** This C program, [hash.c], implements a hash table with
   external chaining.  See http://www.cs.princeton.edu/~appel/HashTables.pdf
   for an introduction to hash tables.  *)

(** 

#include <stddef.h>

extern void * malloc (size_t n);
extern void exit(int n);
extern size_t strlen(const char *str);
extern char *strcpy(char *dest, const char *src);
extern int strcmp(const char *str1, const char *str2);

unsigned int hash (char *s) {
  unsigned int n=0;
  unsigned int i=0;
  int c=s[i];
  while (c) {
    n = n*65599u+(unsigned)c;
    i++;
    c=s[i];
  }
  return n;
}

struct cell {
  char *key;
  unsigned int count;
  struct cell *next;
};

enum {N = 109};

struct hashtable {
  struct cell *buckets[N];
};

char *copy_string (char *s) {
  int i,n = strlen(s)+1;
  char *p = malloc(n);
  if (!p) exit(1);
  strcpy(p,s);
  return p;
}

struct hashtable *new_table (void) {
  int i;
  struct hashtable *p = (struct hashtable * )malloc(sizeof(struct hashtable));
  if (!p) exit(1);
  for (i=0; i<N; i++) p->buckets[i]=NULL;
  return p;
}  

struct cell *new_cell (char *key, int count, struct cell *next) {
  struct cell *p = (struct cell * )malloc(sizeof(struct cell));
  if (!p) exit(1);
  p->key = copy_string(key);
  p->count = count;
  p->next = next;
  return p;
}

unsigned int get (struct hashtable *table, char *s) {
  unsigned int h = hash(s);
  unsigned int b = h % N;
  struct cell *p = table->buckets[b];
  while (p) {
    if (strcmp(p->key, s)==0)
      return p->count;
    p=p->next;
  }
  return 0;
}

void incr_list (struct cell **r0, char *s) {
  struct cell *p, **r;
  for(r=r0; ; r=&p->next) {
    p = *r;
    if (!p) {
      *r = new_cell(s,1,NULL);
      return;
    }
    if (strcmp(p->key, s)==0) {
      p->count++;
      return;
    }
  }
}  

void incr (struct hashtable *table, char *s) {
  unsigned int h = hash(s);
  unsigned int b = h % N;
  incr_list (& table->buckets[b], s);
}
*)

(* ================================================================= *)
(** ** A functional model *)

(** Before we prove the C program correct, we write a functional
 program that models its behavior as closely as possible.  
 The functional program won't be (average) constant time per access,
 like the C program, because it takes linear time to get the nth
 element of a list, while the C program can subscript an array in
 constant time.  But we are not worried about the execution time
 of the functional program; only that it serve as a model
 for specifying the C program. *)


Require Import VST.floyd.functional_base.
Require ReflOmegaCore.

Definition string := list byte.
Instance EqDec_string: EqDec string := list_eq_dec Byte.eq_dec. 

Fixpoint hashfun_aux (h: Z) (s: string) : Z :=
 match s with
 | nil => h
 | c :: s' =>
      hashfun_aux ((h * 65599 + Byte.signed c) mod Int.modulus) s'
end.

Definition hashfun (s: string) := hashfun_aux 0 s.

Definition hashtable_contents := list (list (string * Z)).

Definition N := 109.
Lemma N_eq : N = 109. 
Proof. reflexivity. Qed.
Hint Rewrite N_eq : rep_omega.
Global Opaque N.

Definition empty_table : hashtable_contents :=
  list_repeat (Z.to_nat N) nil.

Fixpoint list_get (s: string) (al: list (string * Z)) : Z :=
  match al with
 | (k,i) :: al' => if eq_dec s k then i else list_get s al'
 | nil => 0
 end.

Fixpoint list_incr (s: string) (al: list (string * Z))
              :  list (string * Z) :=
  match al with
 | (k,i) :: al' => if eq_dec s k 
                      then (k, i +1)::al'
                      else (k,i)::list_incr s al'
 | nil => (s, 1)::nil
 end.

Definition hashtable_get  (s: string) (contents: hashtable_contents) : Z :=
  list_get s (Znth (hashfun s mod (Zlength contents)) contents).

Definition hashtable_incr (s: string) (contents: hashtable_contents)
                      : hashtable_contents :=
  let h := hashfun s mod (Zlength contents)
  in let al := Znth h contents
  in upd_Znth h contents (list_incr s al).

(** **** Exercise: 2 stars (hashfun_inrange)  *)

Lemma mod_range: forall z m: Z, 0 < m -> 0 <= z mod m <= m - 1.
Proof.
  intros.
  assert(0 <= z mod m < m). {
    apply Z.mod_pos_bound; auto.
  }
  omega.
Qed.

Lemma mod_in_range: forall z : Z, 0 <= (z mod Int.modulus) <= Int.max_unsigned.
Proof.
  intros.
  unfold Int.max_unsigned.
  assert(4294967296 = Int.modulus). {
    compute; reflexivity.
  }
  rewrite <- H; clear H.
  apply mod_range; omega.
Qed.

Lemma hashfun_aux_inrange: forall s h, 0 <= h <= Int.max_unsigned ->
                                  0 <= hashfun_aux h s <= Int.max_unsigned.
Proof.
  induction s as [|h t].
  - (* s is nil *)
    intros.
    auto.
  - (* s is h :: t *)
    intros.
    simpl.
    apply IHt.
    apply mod_in_range.
Qed.

Lemma hashfun_inrange: forall s, 0 <= hashfun s <= Int.max_unsigned.
Proof.
  unfold hashfun.
  intros.
  apply hashfun_aux_inrange.
  rep_omega.
Qed.

(** [] *)

(** **** Exercise: 1 star (hashfun_get_unfold)  *)
Lemma hashtable_get_unfold:
 forall sigma (cts: list (list (string * Z) * val)),
 hashtable_get sigma (map fst cts) =
  list_get sigma (Znth (hashfun sigma mod (Zlength cts)) (map fst cts)).
Proof.
  intros.
  unfold hashtable_get.
  rewrite Zlength_map.
  auto.
Qed.

(** [] *)

(** **** Exercise: 2 stars (Zlength_hashtable_incr)  *)
Lemma Zlength_hashtable_incr:
 forall sigma cts, 
      0 < Zlength cts -> 
      Zlength (hashtable_incr sigma cts) = Zlength cts.
Proof.
  intros.
  unfold hashtable_incr.
  rewrite upd_Znth_Zlength; auto.
  apply Z.mod_pos_bound; auto.
Qed.
Hint Rewrite Zlength_hashtable_incr using list_solve : sublist.
(** [] *)

Lemma int_rep_helper: forall x y,
    x mod Int.modulus = y  mod Int.modulus ->
    Int.repr x = Int.repr y.
Proof.
  intros.
  apply Int.eqm_samerepr.
  unfold Int.eqm.
  assert(0 < Int.modulus). {
    rep_omega.
  }
  remember Int.modulus as b.
  clear Heqb.
  unfold Int.eqmod.
  assert(x = b * (x / b) + (x mod b)). {
    apply Z.div_mod.
    rep_omega.
  }
  assert(y = b * (y / b) + (y mod b)). {
    apply Z.div_mod.
    rep_omega.
  }
  assert(y mod b = y - b * (y / b)). {
    omega.
  }
  rewrite H in H1.
  rewrite H3 in H1.
  clear H H2 H3.
  remember (x / b) as xb.
  remember (y / b) as yb.
  clear H0 Heqxb Heqyb.
  exists (xb - yb).
  rewrite Z.mul_comm.
  rewrite H1; clear H1.
  rewrite Z.mul_sub_distr_l.
  rewrite Z.add_sub_assoc.
  omega.
Qed.

Definition hash_step (hin: Z) (c: byte) :=
  ((hin * 65599 + Byte.signed c) mod Int.modulus).

Definition hashfun_aux' (h: Z) (s: string) :=
  fold_left hash_step s h.

Lemma hashfun_aux_equiv:
  forall h s, hashfun_aux h s = hashfun_aux' h s.
Proof.
  intros.
  revert h.
  induction s as [| sh st].
  - (* s is nil *)
    intros.
    unfold hashfun_aux.
    unfold hashfun_aux'.
    simpl.
    auto.
  - (* s is sh :: st *)
    intros.
    unfold hashfun_aux.
    fold hashfun_aux.
    rewrite IHst.
    unfold hashfun_aux'.
    unfold fold_left.
    auto.
Qed.

Definition hash_step' (c: byte) (hin: Z) :=
  hash_step hin c.

Definition hashfun_aux'' (h: Z) (s: string) :=
  fold_right hash_step' h (rev s).

Lemma hashfun_aux_equiv':
  forall h s, hashfun_aux' h s = hashfun_aux'' h s.
Proof.
  intros.
  unfold hashfun_aux'.
  unfold hashfun_aux''.
  assert(hash_step = (fun x y => hash_step' y x)). {
    unfold hash_step'.
    unfold hash_step.
    auto.
  }
  rewrite H.
  rewrite fold_left_rev_right.
  auto.
Qed.

Lemma hashfun_aux_equiv'':
  forall h s, hashfun_aux h s = hashfun_aux'' h s.
Proof.
  intros.
  rewrite hashfun_aux_equiv.
  rewrite hashfun_aux_equiv'.
  auto.
Qed.

Search (rev _ ++ rev _).
Lemma hashfun_snoc0:
  forall (s1 s2: string) (h: Z),
    Int.repr (hashfun_aux h (s1 ++ s2)) =
    Int.repr (hashfun_aux (hashfun_aux h s1) s2).
Proof.
  intros.
  apply int_rep_helper.
  rewrite hashfun_aux_equiv''.
  rewrite hashfun_aux_equiv''.
  rewrite hashfun_aux_equiv''.
  unfold hashfun_aux''.
  rewrite rev_app_distr.
  rewrite fold_right_app.
  auto.
Qed.

Lemma hashfun_snoc:
  forall sigma h lo i,
    0 <= lo ->
    lo <= i < Zlength sigma ->
  Int.repr (hashfun_aux h (sublist lo (i + 1) sigma)) =
  Int.repr (hashfun_aux h (sublist lo i sigma) * 65599 + Byte.signed (Znth i sigma)).
Proof.
  intros.
  assert(sublist lo (i + 1) sigma =
         sublist lo i sigma ++ sublist i (i + 1) sigma). {
    rewrite (sublist_split lo i (i+1)); try omega; auto.
  }
  rewrite H1. clear H1.
  rewrite hashfun_snoc0.
  rewrite sublist_len_1; try omega; auto.
  apply int_rep_helper.
  unfold hashfun_aux at 1.
  rewrite Zmod_mod.
  auto.
Qed.

(** [] *)

(* ================================================================= *)
(** ** Functional model satisfies the high-level specification *)

(** The purpose of a hash table is to implement a finite mapping,
  (a finite function) from keys to values.  We claim that the
  functional model ([empty_table, hashtable_get, hashtable_incr])
  correctly implements the appropriate operations on the abstract
  data type of finite functions.

  We formalize that statement by defining a Module Type: *)

Module Type COUNT_TABLE.
 Parameter table: Type.
 Parameter key : Type.
 Parameter empty: table.
 Parameter get: key -> table -> Z.
 Parameter incr: key -> table -> table.
 Axiom gempty: forall k,   (* get-empty *)
       get k empty = 0.
 Axiom gss: forall k t,      (* get-set-same *)
      get k (incr k t) = 1+(get k t).
 Axiom gso: forall j k t,    (* get-set-other *)
      j <> k -> get j (incr k t) = get j t.
End COUNT_TABLE.

(** This means:  in any [Module] that satisfies this [Module Type],
   there's a type [table] of count-tables,
   and operators [empty], [get], [set] that satisfy the axioms
   [gempty], [gss], and [gso]. *)
  
(* ----------------------------------------------------------------- *)
(** *** A "reference" implementation of COUNT_TABLE *)

(** **** Exercise: 2 stars (FunTable)  *)
(**  It's easy to make a slow implementation of [COUNT_TABLE], using functions. *)

Module FunTable <: COUNT_TABLE.
 Definition table: Type := nat -> Z.
 Definition key : Type := nat.
 Definition empty: table := fun k => 0.
 Definition get (k: key) (t: table) : Z := t k.
 Definition incr (k: key) (t: table) : table :=
    fun k' => if Nat.eqb k' k then 1 + t k' else t k'.
 Lemma gempty: forall k,  get k empty = 0.
 Proof.
   intros.
   unfold get.
   unfold empty.
   auto.
 Qed.
 
 Lemma gss: forall k t,  get k (incr k t) = 1+(get k t).
 Proof.
   intros.
   unfold get.
   unfold incr.
   rewrite Nat.eqb_refl.
   auto.
 Qed.
 
 Lemma gso: forall j k t,  j <> k -> get j (incr k t) = get j t.
 Proof.
   intros.
   unfold get.
   unfold incr.
   rewrite <- Nat.eqb_neq in H.
   rewrite H.
   auto.
 Qed.
 
End FunTable.
(** [] *)

(* ----------------------------------------------------------------- *)
(** *** Demonstration that hash tables implement COUNT_TABLE *)

(** **** Exercise: 3 stars (IntHashTable)  *)
(**  Now we make a "fast" implementation using hash tables.  We
  put "fast" in quotes because, unlike the imperative implementation,
 the purely functional implementation takes linear time, not constant time,
 to select the the i'th bucket.  That is, [Znth i al] takes time proportional to [i].
 But that is no problem, because we are not using [hashtable_get] and
 [hashtable_incr] as our real implementation; they are serving as the 
 _functional model_ of the fast implementation in C.  *)

Module IntHashTable <: COUNT_TABLE.
 Definition hashtable_invariant (cts: hashtable_contents) : Prop :=
  Zlength cts = N /\
  forall i, 0 <= i < N ->
             list_norepet (map fst (Znth i cts))
             /\ Forall (fun s => hashfun s mod N = i) (map fst (Znth i cts)).
 Definition table := sig hashtable_invariant.
 Definition key := string.

 Lemma empty_invariant: hashtable_invariant empty_table.
 Proof.
   
   unfold empty_table.
   unfold hashtable_invariant;split.
   { (* Length *)
     rewrite  Zlength_correct.
     rewrite length_list_repeat.
     rewrite Z2Nat.id; auto.
     rewrite N_eq.
     omega.
   }
   
   { intros;split.
     { (* list_norepet (map fst (Znth i (list_repeat (Z.to_nat N) nil))) *)
       rewrite Znth_list_repeat_inrange;auto.
       simpl.
       apply list_norepet_nil.
     }
     { (* Forall (fun s : string => hashfun s mod N = i)
          (map fst (Znth i (list_repeat (Z.to_nat N) nil))) *)  
       rewrite Znth_list_repeat_inrange;auto.
       simpl.
       constructor.
     }
   }
 Qed.

 Lemma list_incr_in_existing:
   forall (s: string) (al: list (string * Z)),
     (In s (map fst al)) ->
     (map fst (list_incr s al)) = (map fst al).
 Proof.
   intros.
   induction al as [|hd tl].
   - (* al is nil *)
     inversion H.
   - (* al is hd :: tl *)
     destruct hd as [hs hz].
     simpl in *.
     destruct (eq_dec s hs) eqn: dshs.
     * simpl;auto.
     * simpl.
       f_equal.
       apply IHtl.
       destruct H; auto.
       subst hs. contradiction.
 Qed.
 
 Lemma list_incr_not_in_existing:
   forall (s: string) (al: list (string * Z)),
     ~ (In s (map fst al)) ->
     (map fst (list_incr s al)) = (map fst al) ++ s :: nil.
 Proof.
   intros.
   revert H.
   induction al as [|hd tl].
   - (* al is nil *)
     intros.
     simpl; auto.
   - (* al is hd :: tl *)
     intros.
     destruct hd as [hs hz].
     simpl in *.
     assert(hs <> s /\ ~ In s (map fst tl)). {
        split.
        - unfold not.
          intro.
          subst hs.
          apply H.
          left;auto.
        - intro.
          apply H.
          right;auto.
     }
     destruct H0.
     destruct (eq_dec s hs) eqn: dshs.
     * subst hs.
       contradiction.
     * simpl in *.
       f_equal.
       apply IHtl;auto.
 Qed.

 Lemma list_incr_not_in:
   forall (s hstring : string) (t : list (string * Z)),
     ~ In hstring (map fst t) ->
     s <> hstring -> ~ In hstring (map fst (list_incr s t)).
 Proof.
   intros s hstring t H2 n.
   assert(forall l l' : list byte, {l = l'} + {l <> l'}). {
     apply list_eq_dec.
     apply Byte.eq_dec.
   }
   destruct (ListDec.In_dec H s (map fst t)) eqn: si.
   - rewrite list_incr_in_existing; auto.
   - rewrite list_incr_not_in_existing; auto.
     intro.
     remember (map fst t) as ts.
     remember (s :: nil) as ssingle.
     Search app.
     apply in_app_or in H0.
     destruct H0; try contradiction.
     subst ssingle.
     simpl in *.
     destruct H0; try contradiction.
 Qed.
 
 Lemma list_incr_nodup:
   forall (s: string) (al: list (string * Z)),
     list_norepet (map fst al) -> list_norepet (map fst (list_incr s al)).
 Proof.
   intros.
   induction al as [|h t].
   - (* al is nil *)
     vm_compute.
     assert(~In s nil). {
       auto.
     }
     apply (list_norepet_cons s H0).
     constructor.
   - inversion H.
     simpl.
     destruct h as [hstring hcount].
     simpl in *.
     destruct (eq_dec s hstring) eqn: sh.
     * simpl.
       apply list_norepet_cons; auto.
     * simpl in *.
       apply list_norepet_cons; auto.
       apply list_incr_not_in; auto.
 Qed.

Check ReflOmegaCore.ZOmega.IP.beq_reflect.
 Lemma incr_invariant:
   forall k cts, hashtable_invariant cts -> hashtable_invariant (hashtable_incr k cts).
 Proof.
   intros.
   destruct H.
   unfold hashtable_incr.
   split.
   { (* Length unchanged *)
     rewrite upd_Znth_Zlength; auto.
     assert(forall (A: Type) (l: list A), 0 <= Zlength l). {
       intros.
       apply (Zlength_nonneg l).
     }
     specialize (H1 (list (string * Z)) cts).
     rewrite H in *.
     rewrite N_eq in *.
     generalize (hashfun k).
     intros.
     apply Z.mod_pos_bound; omega.
   }
   intros.
   split.
   {
     (* list_norepet *)
     remember (hashfun k mod Zlength cts) as bucket.
     destruct (ReflOmegaCore.ZOmega.IP.beq_reflect bucket i).
     { (* bucket = i *)
       subst i.
       rewrite <- H in *.
       rewrite upd_Znth_same; auto.
       apply list_incr_nodup.
       specialize (H0 bucket).
       apply H0 in H1.
       destruct H1; auto.
     }
     { (* bucket != i *)
       rewrite <- H in *.
       rewrite upd_Znth_diff; auto.
       specialize (H0 i).
       apply H0 in H1.
       destruct H1;auto.
       subst bucket.
       rewrite H in *.
       rewrite N_eq in *.
       apply Z.mod_pos_bound.
       omega.
     }
   }
   { (* All the strings in the ith bucket hash to i. *)
     remember (hashfun k mod Zlength cts) as bucket.
     destruct (ReflOmegaCore.ZOmega.IP.beq_reflect bucket i).
     { (* bucket = i *)
       subst i.
       rewrite <- H in *.
       rewrite upd_Znth_same; auto.
       assert(forall l l' : list byte, {l = l'} + {l <> l'}). {
         apply list_eq_dec.
         apply Byte.eq_dec.
       }
       destruct (ListDec.In_dec H2 k (map fst (Znth bucket cts))) eqn: si.
       { (* k is in the bucket already. *)
         rewrite list_incr_in_existing; auto.
         specialize (H0 bucket).
         apply H0 in H1.
         destruct H1; auto.
       }
       { (* k is not in the bucket. *)
         rewrite list_incr_not_in_existing; auto.
         specialize (H0 bucket).
         apply H0 in H1.
         destruct H1; auto.
         apply Forall_app.
         split; auto.
       }
     }
     { (* bucket != i *)
       rewrite <- H in *.
       rewrite upd_Znth_diff; auto.
       specialize (H0 i).
       apply H0 in H1.
       destruct H1;auto.
       subst bucket.
       rewrite H in *.
       rewrite N_eq in *.
       apply Z.mod_pos_bound.
       omega.
     }
   }
 Qed.
 

 Definition empty : table := exist _ _ empty_invariant.
 Definition get : key -> table -> Z := fun k tbl => hashtable_get k (proj1_sig tbl).
 Definition incr : key -> table -> table := 
       fun k tbl => exist _ _ (incr_invariant k _ (proj2_sig tbl)).


 Theorem gempty: forall k, get k empty = 0.
 Proof.
   intros.
   unfold empty.
   unfold empty_table.
   unfold get.
   simpl.
   unfold hashtable_get.
   rewrite  Zlength_correct.
   rewrite length_list_repeat.
   assert(0 < N). {
     rewrite N_eq;omega.
   }
   rewrite Z2Nat.id by omega.
   remember (hashfun k mod N) as bucket.
   assert(0 <= bucket < N). {
     subst bucket.
     apply Z.mod_pos_bound. auto.
   }
   rewrite Znth_list_repeat_inrange by auto.
   simpl; auto.
 Qed.
 
 Lemma get_incr_result:
   forall (k : key) (bklist : list (string * Z)),
     list_get k (list_incr k bklist) = 1 + list_get k bklist.
 Proof.
   intros k bklist.
   induction bklist as [|hd tl].
   - (* bklist is  nil *)
     simpl.
     rewrite pred_dec_true;auto.
   - (* inductive case *)
     simpl.
     destruct hd as [hk hz].
     destruct (eq_dec k hk).
     * rewrite pred_dec_true; auto.
       unfold list_get.
       rewrite pred_dec_true; auto.
       rewrite pred_dec_true; auto.
       rewrite Z.add_comm.
       reflexivity.
     * rewrite pred_dec_false; auto.
       unfold list_get.
       rewrite pred_dec_false; auto.
       rewrite pred_dec_false; auto.
 Qed.

 Lemma get_incr_diff_result:
   forall (j k : key) (bklist : list (string * Z)),
     j <> k -> 
     list_get j (list_incr k bklist) = list_get j bklist.
 Proof.
   intros j k bklist H.
   induction bklist as [|hd tl].
   - (* bklist is  nil *)
     simpl.
     rewrite pred_dec_false;auto.
   - (* inductive case *)
     simpl.
     destruct hd as [hk hz].
     destruct (eq_dec j hk).
     * (* j = hk *)
       rewrite (pred_dec_true (eq_dec j hk)); auto.
       rewrite pred_dec_false.
       simpl.
       rewrite pred_dec_true; auto.
       intro.
       subst j k.
       contradiction.
     * (* j <> hk *)
       rewrite (pred_dec_false (eq_dec j hk)); auto.
       destruct (eq_dec k hk).
     { (* k = hk *)
       rewrite pred_dec_true.
       simpl.
       rewrite pred_dec_false; auto.
       assumption.
     }
     { (* k <> hk *)
       rewrite pred_dec_false; auto.
       simpl.
       rewrite pred_dec_false; auto.
     }
 Qed.

 Theorem gss: forall k t,  get k (incr k t) =  1 + (get k t).
 Proof.
   intros.
   unfold incr.
   unfold get.
   unfold hashtable_get.
   unfold hashtable_incr.
   destruct t.
   simpl.
   destruct h.
   rewrite H in *.
   rewrite N_eq in *.
   remember (hashfun k mod 109) as bk.
   assert(forall z : Z,
             1 + z =
             (match z with
             | 0%Z => 1%Z
             | Z.pos y' => Z.pos match y' with
                                | q~1 => (Pos.succ q)~0
                                | q~0 => q~1
                                | 1 => 2
                                end
             | Z.neg y' => Z.pos_sub 1 y'
              end)%positive). {
     reflexivity.
   }
   rewrite <- H1.
   assert(0 <= bk < Zlength x). {
     rewrite Heqbk.
     rewrite H.
     apply Z.mod_pos_bound; omega.
   }
   rewrite upd_Znth_Zlength;auto.
   rewrite H.
   rewrite <- Heqbk.
   rewrite upd_Znth_same;auto.
   Search list_incr.
   remember (Znth bk x) as bklist.
   rewrite H in *.
   apply get_incr_result.
 Qed.
 
 Theorem gso: forall j k t,    (* get-set-other *)
      j <> k -> get j (incr k t) = get j t.
 Proof.
   intros j k t Hne.
   unfold incr.
   unfold get.
   unfold hashtable_get.
   unfold hashtable_incr.
   destruct t.
   simpl.
   destruct h.
   rewrite H in *.
   rewrite N_eq in *.
   remember (hashfun k mod 109) as bk.
   assert(0 <= bk < Zlength x). {
     rewrite Heqbk.
     rewrite H.
     apply Z.mod_pos_bound; omega.
   }
   rewrite upd_Znth_Zlength;auto.
   remember (hashfun j mod 109) as bj.
   assert(0 <= bj < Zlength x). {
     rewrite Heqbj.
     rewrite H.
     apply Z.mod_pos_bound; omega.
   }
   rewrite H.
   rewrite <- Heqbj.
   destruct (Z.eq_dec bj bk) eqn:eqbjbk.
   { (* bj = bk *)
     rewrite e.
     rewrite upd_Znth_same.
     apply get_incr_diff_result; auto.
     assumption.
   }
   { (* bj <> bk *)
     rewrite upd_Znth_diff; auto.
   }
 Qed.

(** [] *)

End IntHashTable.
