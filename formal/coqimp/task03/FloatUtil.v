From compcert Require Import Floats.
Require Import BinNums.
Require Import Coq.PArith.BinPosDef.

Definition doubling(z:BinNums.Z)(n:nat) : nat :=
  match z with
  |Z0 => n
  |Zpos p => (BinPos.Pos.iter_op mult p 2) * n
  |Zneg p => (fix h(q:positive)(k:nat) : nat :=
   match q with
             |xH => Nat.div2 n
             |xO r => h r (h r k)
             |xI r => Nat.div2 (h r (h r k))
             end) p n
  end.

  Lemma doublesN : forall n:nat, forall z:positive, doubling (BinInt.Z.add (Zpos z) (Zpos xH)) n = 2 * (doubling (Zpos z) n).
    + induction z as [q|q|].
    - intros.
      assert (H := fun k:nat =>  BinPos.Pos.iter_op_succ _ Nat.mul PeanoNat.Nat.mul_assoc q k).
      simpl in *.
      rewrite (H _)...
      simpl in *. 
      rewrite (PeanoNat.Nat.add_comm _ 0); simpl...
      rewrite (PeanoNat.Nat.add_comm _ 0)...
      simpl.
      rewrite (PeanoNat.Nat.add_assoc)...
      rewrite (PeanoNat.Nat.mul_add_distr_r)...
      trivial.
    - intros.
      simpl in *.
      rewrite (PeanoNat.Nat.add_comm _ 0) in *...
      simpl in *.
      rewrite (PeanoNat.Nat.add_comm _ 0) in *...
      simpl in*.
      apply PeanoNat.Nat.mul_add_distr_r.
    - simpl.
      auto with *. 
  Defined.
  Check @Fcore_FLT.FLT_exp.
Fixpoint mb_fuel (mantissa:positive)(exponent:BinNums.Z) (fuel:nat) (emax prec:BinNums.Z) : (positive * BinNums.Z)%type :=  
  match fuel with
  |O => (mantissa, exponent)
  |S k => match BinInt.Z.compare (Fcore_FLT.FLT_exp (BinInt.Z.sub (BinInt.Z.sub (Zpos 3) emax prec) prec
                                  (BinInt.Z.add (Zpos (Fcore_digits.digits2_pos mantissa)) exponent)) exponent with
          |Eq => (mantissa, exponent)
          |Lt => match (BinInt.Z.ltb prec (Fcore_digits.digits2_pos mantissa))
                         |true => mb_fuel 
          |Gt => (mantissa, exponent)
          end
  end.
  Check (Fcore_FLT.FLT_exp).
  Check (BinInt.Z.max).
Fixpoint eb_fuel (mantissa exponent fuel:nat)(emax prec:BinNums.Z) : (positive * BinNums.Z)%type :=
  match fuel with
  |0 => mb_fuel (Pos.of_nat mantissa) (BinIntDef.Z.opp (BinIntDef.Z.of_nat exponent)) (Pos.to_nat (Fcore_digits.digits2_pos (Pos.of_nat mantissa))) emax prec
  |S k => match (BinInt.Z.leb (BinIntDef.Z.opp (BinIntDef.Z.of_nat exponent)) (BinInt.Z.sub emax prec)) with
          |true => mb_fuel (Pos.of_nat mantissa) (BinIntDef.Z.opp (BinIntDef.Z.of_nat exponent)) (Pos.to_nat (Fcore_digits.digits2_pos (Pos.of_nat mantissa))) emax prec
          |false => (eb_fuel ((doubling (BinInt.Z.sub emax prec)) mantissa) (exponent + (BinIntDef.Z.to_nat (BinInt.Z.sub emax prec))) k emax prec)
         end
  end.

Definition exponent_builder (mantissa exponent:nat) : BinNums.Z := snd (eb_fuel mantissa exponent exponent (Zpos 1024) (Zpos 53)).

Definition mantissa_builder (mantissa exponent:nat) : BinNums.positive := fst (eb_fuel mantissa exponent exponent (Zpos 1024) (Zpos 53)).

(*Computes +- mantissa * 2^-exponent*)
Definition buildFloat(sign : bool)(mantissa exponent: nat) : float.
  destruct mantissa as [|m] eqn:Em.
  apply Fappli_IEEE.B754_zero.
  apply sign.
  cbv.
  
  apply (Fappli_IEEE.B754_finite _ _ sign (mantissa_builder (S m) exponent) (exponent_builder (S m) exponent)).
  simpl.
  unfold Fappli_IEEE.bounded.
  unfold Fappli_IEEE.canonic_mantissa.
  unfold exponent_builder.
  unfold mantissa_builder.
  unfold eb_fuel.
  simpl.
  destruct exponent.
  simpl.
  unfold BinInt.Z.leb.
  simpl.
  destruct m.
  simpl.
  