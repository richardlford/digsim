(* Verify simple double floating add. *)
Require Import VST.floyd.proofauto.

Record Model : Set :=
  mkModel {

      x : float;
      xd : float
    }.

Record Driver : Set :=
  mkDriver {
      done : bool;
      s : Model;
      states : list (float -> Model -> Model)
    }.

Record Event : Set :=
  mkEvent {
      etime : float;
      ename : string;
      efunc : Driver -> Driver
    }.

Definition Events := list Event.

Fixpoint merge (l1 l2 : events) :=
  let fix merge_aux l2 :=
  match l1, l2 with
  | [], _ => l2
  | _, [] => l1
  | a1::l1', a2::l2' =>
      if a1 <=? a2 then a1 :: merge l1' l2 else a2 :: merge_aux l2'
  end
  in merge_aux l2.

Float.add
Definition terminate (d: Driver) :=
  {| done := true; s := d.(s); states := d.(states) |}.

Section Test.

End Test.


