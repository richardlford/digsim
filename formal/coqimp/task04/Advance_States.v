From compcert Require Import Floats Fcalc_round.
Require Import digsim.State.
Require Import digsim.Differential_Equations.
Require Import digsim.Termination_Conditions.
Require Import digsim.Events.
Require Import Coq.Lists.List.
Require Import Coq.Vectors.Vector.
Require Import Coq.Numbers.BinNums.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Task.float_text_io.

Definition advancePosition(st:State)(index : Fin.t Ndes)(Dt:float) : State := (Set_x st index (Float.add (Vector.nth (x st) index) (Float.mul (nth (xDot st) index) (Dt)))) .

Definition advanceXDot(st:State)(index : Fin.t Ndes)(Dt:float) : State := (Set_xDot st index (Float.add (nth (xDot st) index) (Float.mul (nth (Xdd st) index) (Dt)))) .

Definition advanceIndex(st:State)(index : Fin.t Ndes)(Dt:float) : State := advanceXDot (advancePosition st index Dt) index Dt.

Fixpoint allIndices (size:nat) : t (Fin.t size) size :=
  match size with
  |0 => nil _
  |S n => cons _ Fin.F1 _ (map Fin.FS (allIndices n))
  end.

Lemma actionOnIn : forall A B:Type, forall f:A -> B, forall n:nat, forall v:t A n, forall a:A, (In a v) -> (In (f a) (map f v)).  
intros.
induction H.
simpl.
constructor 1.
constructor 2.
apply IHIn.
Defined.

Theorem actuallyAllIndices: forall n:nat, forall index:Fin.t n, In index (allIndices n).
  intros.
  induction index.
  constructor 1.
  unfold allIndices.
  fold allIndices.
  constructor 2.
  apply actionOnIn.
  apply IHindex.
Defined.

Fixpoint advanceIndices(st:State){n:nat}(indices : t (Fin.t Ndes) n)(Dt:float) : State :=
  match indices with
  |nil => st
  |cons a _ j => advanceIndex (advanceIndices st j Dt) a Dt
  end.


Definition advanceTime(st:State)(Dt:float) : State := Set_Time st (Float.add (Time st) (Dt)).

Definition tab := String "009"%char "".

Definition printTimePos(st:State) : State := (print st ((FloatIO.float_to_string 4 3 (Time st)) ++ tab ++ (FloatIO.float_to_string 4 3 (Vector.nth (x st) Fin.F1)) ++ tab ++ (FloatIO.float_to_string 4 3 (Vector.nth (xDot st) Fin.F1)))).

Definition defaultAdvance(st:State)(Dt:float) : State := printTimePos((advanceTime (advanceIndices st (allIndices Ndes) Dt) Dt)).

Definition advance(st:State) : State :=
  match (nextEvent st) with
  |None => defaultAdvance st (DtMax st)
  |Some (advSt, (t, (cmd, args))) => match  (executeEvent cmd args (print advSt ("Event: " ++ cmd ++ "@" ++ (FloatIO.float_to_string 4 3 (Time advSt))))) with
                                    |inl st' =>    match (Float.cmp Integers.Cge (Float.sub (Time st) t) (DtMin st)) with
                                                  |false => defaultAdvance st' (DtMin st')
                                                  |true => defaultAdvance st' (Float.sub (Time st') t)
                                                  end
                                    |inr s => defaultAdvance (print st s) (DtMax st)
                                    end
  end.

Definition neededFuel(st:State) : nat := match Float.to_int (Float.div (Float.sub (Tstop st) (Time st)) (DtMin st)) with
                                       |Some k => match Integers.Int.intval k with
                                                 |Zpos p => BinPos.Pos.to_nat p
                                                 |_ => 0
                                                 end
                                       |_ => 0
                                       end.

Open Scope string_scope.

Fixpoint run_fuel(st:State)(fuel:nat) := match fuel with
                                       |0 => st
                                       |S k => match Quit st with
                                              |false => run_fuel (advance st) k
                                              |true => st
                                              end
                                       end.

Definition run(st:State) : State := run_fuel st (neededFuel st).
