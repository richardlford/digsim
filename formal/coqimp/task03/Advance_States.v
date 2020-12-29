From compcert Require Import Floats Fcalc_round.
Require Import digsim.State.
Require Import digsim.Differential_Equations.
Require Import digsim.Termination_Conditions.
Require Import Coq.Vectors.Vector.
Require Import Coq.Numbers.BinNums.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Task.float_text_io.

Definition advancePosition(st:State)(index : Fin.t Ndes) : State := (Set_x st index (Float.add (nth (x st) index) (Float.mul (nth (xDot st) index) (Dt st)))) .

Definition advanceXDot(st:State)(index : Fin.t Ndes) : State := (Set_xDot st index (Float.add (nth (xDot st) index) (Float.mul (nth (Xdd st) index) (Dt st)))) .

Definition advanceIndex(st:State)(index : Fin.t Ndes) : State := advanceXDot (advancePosition st index) index.

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

Fixpoint advanceIndices(st:State){n:nat}(indices : t (Fin.t Ndes) n) : State :=
  match indices with
  |nil => st
  |cons a _ j => advanceIndex (advanceIndices st j) a
  end.


Definition advanceTime(st:State) : State := Set_Time st (Float.add (Time st) (Dt st)).

Definition advance(st:State) : State := advanceTime (advanceIndices st (allIndices Ndes)).

Definition neededFuel(st:State) : nat := match Float.to_int (Float.div (Float.sub (Tstop st) (Time st)) (Dt  st)) with
                                       |Some k => match Integers.Int.intval k with
                                                 |Zpos p => BinPos.Pos.to_nat p
                                                 |_ => 0
                                                 end
                                       |_ => 0
                                       end.

Open Scope string_scope.

Definition tab : string := String "009"%char "".

Definition printState (st:State) := print st ((FloatIO.float_to_string 4 3 (Time st)) ++ tab  ++ (FloatIO.float_to_string 4 3 (Vector.nth (x st) Fin.F1)) ++ tab ++ (FloatIO.float_to_string 4 3 (Vector.nth (xDot st) Fin.F1))).

Fixpoint run_fuel(st:State)(fuel:nat) := match fuel with
                                       |0 => (printState st)
                                       |S k => match Quit st with
                                              |false => advance (printState st)
                                              |true => (printState st)
                                              end
                                       end.
Definition run(st:State) : State := run_fuel st (neededFuel st).
