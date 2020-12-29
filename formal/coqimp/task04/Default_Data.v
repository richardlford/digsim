From compcert Require Import Floats.
Require Import digsim.State.
Require Import Coq.Vectors.Vector.
Require Import Coq.ZArith.ZArith.
Require Import Coq.Strings.String.

Close Scope nat_scope.
Global Open Scope Z_scope.

(*4503599627370496 = 2^52. 2^52 * 2^-52 = 2^0 = 1*)
Definition one := (Fappli_IEEE.B754_finite 53 1024 false 4503599627370496 (-52)
                                           (proj1 (Fappli_IEEE.binary_round_correct 53 1024 eq_refl eq_refl Fappli_IEEE.mode_NE false 1 0))).

Fixpoint nat2float(n:nat) : float :=
  match n with
  |O => Float.zero
  |S k => Float.add (nat2float k) one
  end.

Definition rational2float (n d:nat) : float := Float.div (nat2float n) (nat2float d).

Definition Default_X_IC : float := Float.zero.
Definition Default_Xd_IC : float := Float.zero.

Fixpoint constantVector {A:Type}(a:A)(n:nat) : t A n :=
  match n with
  |O => nil A
  |S k => cons _ a _ (constantVector a k)
  end.

Definition Default_Data : State := mkState
                                     (constantVector Default_X_IC Ndes)
                                     (constantVector Default_Xd_IC Ndes)
                                     
                                     Float.zero
                                     Float.zero
                                     (rational2float 5 2)

                                      (rational2float 1 10000)
                                     (rational2float 1 100)
                                  
                                     (rational2float 888 100)
                                     (rational2float 988 100)

                                     (nat2float 1)
                                     (rational2float 3947 100)
                                     Default_X_IC
                                     Default_Xd_IC

                                     ""
                                     List.nil.