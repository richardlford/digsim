(** * Formulation of Euler Numerical Integration *)
Require Import VST.floyd.proofauto.
Require Import Coquelicot.Coquelicot.
Require Import Reals.
Require Import Digsim.float_text_io.
Open Scope R_scope.

Section NumericalIntegration.
  Variable xx : R.
  Variable f1 f2 : R -> R.

(* Definition filterdiff (f : U -> V) F (l : U -> V) :=
   is_linear l /\ forall x, is_filter_lim F x ->
   is_domin F (fun y : U => minus y x) (fun y => minus (minus (f y) (f x)) (l (minus y x))).
 *)
  
  Print filterdiff.
  (*
    filterdiff = 
    fun (K : AbsRing) (U V : NormedModule K) (f : U -> V) (F : (U -> Prop) -> Prop) (l : U -> V) =>
    is_linear l /\
    (forall x : U,
     is_filter_lim F x ->
     is_domin F (fun y : U => minus y x) (fun y : U => minus (minus (f y) (f x)) (l (minus y x))))
         : forall (K : AbsRing) (U V : NormedModule K), (U -> V) -> ((U -> Prop) -> Prop) -> (U -> V) -> Prop

    Arguments K, U, V are implicit and maximally inserted
    Argument scopes are [_ _ _ function_scope function_scope function_scope]

   *)

  Variable myfilter : (R -> Prop) -> Prop.
  
  Definition f2isf1derive := filterdiff f1 myfilter f2.

  Print Derive.
  (* 
    Derive = 
    fun (f : R -> R) (x : R) => real (Lim (fun h : R => (f (x + h) - f x) / h) 0)
         : (R -> R) -> R -> R

    Argument scopes are [function_scope R_scope]
   *)
  
  Definition f2isf1derive' := f2 = Derive f1.

  Print is_derive.

  (*
    is_derive = 
    fun (K : AbsRing) (V : NormedModule K) (f : K -> V) (x : K) (l : V) =>
    filterdiff f (locally x) (fun y : K => scal y l)
         : forall (K : AbsRing) (V : NormedModule K), (K -> V) -> K -> V -> Prop

    Arguments K, V are implicit and maximally inserted
    Argument scopes are [_ _ function_scope _ _]
   *)
  
End NumericalIntegration.

