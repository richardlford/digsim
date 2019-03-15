(** DigSim

   DigSim provides a Coq architecture required to simulate continuous systems
   described by sets of simultaneous first-order differential equations.

     xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;


 ** _CoqProject config file contents

   -R ~/opt/lib/compcert/coq compcert -R . Top

 *)

Require Import DigUty.float_text_io.
Require Import DigUty.Monad.

From compcert Require Import Floats.
Import Float.
From compcert Require Import Fappli_IEEE.
From compcert Require Import Integers.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import BinNums.
Require Import Coq.QArith.QArith_base.
Require Import Strings.String.
Require Import Strings.Ascii.
Require Import Coq.ZArith.Znat.
Require Import Recdef.
From compcert Require Import Maps.
Import FloatIO.
From RecordUpdate Require Import RecordUpdate.

(* Inductive type used to name state variables. *)
Inductive StateVar : Set := 
| svT
| svX
| svXD
| svXDD
| svCOEFF_OF_REST
| svGRAVITY
| svT_STOP
| svDT
| svDT_MAX
| svDT_MIN
| svDT_PRINT.

Lemma state_var_eq: forall (r1 r2: StateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

From compcert Require Import Decidableplus.

Instance Decidable_eq_sv : forall (x y: StateVar), Decidable (eq x y) := Decidable_eq state_var_eq.

(* For diagnostic purposes we want to map from the positive id for a
   StateVar to the StateVar. 
 *)
Definition PosToStateVarTree := PTree.t StateVar.
Definition EmptyPosToStateVarTree := PTree.empty StateVar.

Definition SvIndex (sv: StateVar) : positive :=
  match sv with
  | svT => 1
  | svX => 2
  | svXD => 3
  | svXDD => 4
  | svCOEFF_OF_REST => 5
  | svGRAVITY => 6
  | svT_STOP => 7
  | svDT => 8
  | svDT_MAX => 9
  | svDT_MIN => 10
  | svDT_PRINT => 11
  end.

Definition SvStrList :=
  [
    (svT, "T");
    (svX, "X");
    (svXD, "XD");
    (svXDD, "XDD");
    (svCOEFF_OF_REST, "COEFF_OF_REST");
    (svGRAVITY, "GRAVITY");
    (svT_STOP, "T_STOP");
    (svDT, "DT");
    (svDT_MAX, "DT_MAX");
    (svDT_MIN, "DT_MIN");
    (svDT_PRINT, "DT_PRINT")
  ]%positive.

Fixpoint UpdatePosToStateVarTree (svIdStrList: list (StateVar * string))
         (tree: PosToStateVarTree) : PosToStateVarTree :=
  match svIdStrList with
  | nil => tree
  | cons (sv, str) xtail =>
    let tree1 := PTree.set (SvIndex sv) sv tree in
    UpdatePosToStateVarTree xtail tree1
  end.

Definition posToStateVarTree :=
  Eval compute in UpdatePosToStateVarTree SvStrList EmptyPosToStateVarTree.

Definition posToStateVar (p: positive) := PTree.get p posToStateVarTree.

(*
Compute PTree.elements posToStateVarTree.
Compute posToStateVar 3.
*)

Module SvIndex <: Maps.INDEXED_TYPE.
  Definition t := StateVar.
  Definition index := SvIndex.
  Definition eq := state_var_eq.
  Lemma index_inj:
    forall r1 r2, index r1 = index r2 -> r1 = r2.
  Proof.
    intros.
    destruct r1 eqn: r1eq; destruct r2 eqn: r2eq; simpl in *;try reflexivity;
      unfold index in H; simpl in H; inversion H.
  Qed.
End SvIndex.

Module SvTree := Maps.ITree(SvIndex).

Definition StringSvTree := SvTree.t string.
Definition emptyStringPTree := SvTree.empty string.

Fixpoint UpdateStringSvTree (valList: list (StateVar * string)) (intree: StringSvTree) :=
  match valList with
  | nil => intree
  | cons (sv, val) vltail =>
    let tree1 := SvTree.set sv val intree in
    UpdateStringSvTree vltail tree1
  end.

Definition svToStringTree :=
  Eval compute in UpdateStringSvTree SvStrList emptyStringPTree.

Definition svToStrOpt (sv: StateVar) : option string :=
  SvTree.get sv svToStringTree.

Lemma svToStrComplete:
  forall sv : StateVar, exists s : string,
      svToStrOpt sv = Some s.
Proof.
  intros.
  destruct sv; unfold svToStrOpt;compute; eexists; f_equal.
Qed.

(* Since we have proven that svToStrOpt will always give a 
   result, define a function that extracts that value,
   and in the impossible case returns the empty string.
*)
Definition svToStr (sv: StateVar) : string :=
  match svToStrOpt sv with
  | Some x => x
  | None => ""
  end.

(*
Compute svToStr svDT.
Compute svToStr svDT_PRINT.
*)

Definition FloatSvTree := SvTree.t float.
Definition emptyFloatPTree := SvTree.empty float.

Fixpoint UpdateFloatSvTree (valList: list (StateVar * string)) (intree: FloatSvTree) :=
  match valList with
  | nil => intree
  | cons (sv, str) vltail =>
    let tree1 :=
    match strToFloat str with
    | Some val => SvTree.set sv val intree
    | None => intree
    end in
    UpdateFloatSvTree vltail tree1
  end.

Definition initialValues :=
  [
    (svT,             "0.0");
    (svX,             "10.0");
    (svXD,            "0.0");
    (svXDD,           "0.0");
    (svCOEFF_OF_REST, "0.80");
    (svGRAVITY,       "9.88");
    (svT_STOP,        "10.0");
    (svDT,            "0.01");
    (svDT_MAX,        "0.005");
    (svDT_MIN,        "0.001");
    (svDT_PRINT,      "0.01")
  ].

Definition state0 := Eval compute in UpdateFloatSvTree initialValues emptyFloatPTree.

Definition svToFloat0 (sv: StateVar) := SvTree.get sv state0.

Definition svToFloatStr0 (sv: StateVar) :=
  match svToFloat0 sv with
  | Some x => float_to_string 8 4 x
  | None => "None"
  end.

Compute svToFloatStr0 svGRAVITY.

Lemma state0Complete:
  forall sv: StateVar, exists f: float,
      SvTree.get sv state0 = Some f.
Proof.
  intros.
  destruct sv;compute; eexists; f_equal.
Qed.

(*
Compute SvTree.get svXD (SvTree.set svXD Float.zero emptyFloatPTree).
*)

Definition modelOutputs : list StateVar := [svT; svX; svXD].
Print modelOutputs.



Definition ident := term.
    

Definition _time: ident := 1.


(** DigSim - Mass Spring Damper
 *)
Definition time0               := Z_to_float 0                    . (* 0.0          simulation time [sec]                      *)
Definition x_ic                := Z_to_float 0                    . (* 0.0          initial position of suspended mass [m]     *)
Definition xd_ic               := Z_to_float 0                    . (* 0.0          initial velocity of suspended mass [m/sec] *)
Definition mass                := Z_to_float 1                    . (* 1.0    1#1   mass suspended from spring [kg]            *)
Definition tstop               := fraction_to_float  25  1        . (* 2.5    2#5   simulation stop time [sec]                 *)
Definition dt                  := fraction_to_float   1  2        . (* 0.01   1#100 simulation time step [sec]                 *)
Definition damping_coefficient := fraction_to_float 888  2        . (* 8.88 222#25  damping force per velocity [n/m/s]         *)
Definition gravity             := fraction_to_float 988  2        . (* 9.88 247#25  acceleration due to gravity [m/sec**2]     *)
Definition spring_coefficient  := fraction_to_float 3947 2        . (* 39.47 3947#100 restoring force per position [n/m]       *)

Open Scope Z.

Definition xdd x xd : float :=
  sub ( div (neg (add (mul spring_coefficient x)  (mul damping_coefficient xd)) ) mass) gravity.

Definition step (t x xd : float) : (float * float * float) :=
  let xdd' := xdd x xd             in
  let x'   := add x  (mul xd   dt) in
  let xd'  := add xd (mul xdd' dt) in
  let t'   := add t dt             in
  (t', x', xd').

Fixpoint advance (fuel : nat) (t x xd : float) : list (list float) :=
match fuel with
|  O   => nil
|  S n =>
   let '(t', x', xd') := (step t x xd) in
   (t' :: x' :: xd' :: nil) :: (advance n t' x' xd')
end.

Definition width := 15%nat.
Definition fdigs := 6%nat.
Definition f2s := float_to_string width fdigs.
Definition fl2s := float_list_to_string width fdigs true.

Definition digsim : string := nl ++ (fl2s (time0 :: x_ic :: xd_ic :: nil)) ++
                        (concat "" (map fl2s (advance 250 time0 x_ic xd_ic))) ++ nl.

Set Printing Depth 500.
Set Printing Width 0.
Compute digsim.
