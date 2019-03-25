(** DigSim

   DigSim provides a Coq architecture required to simulate continuous systems
   described by sets of simultaneous first-order differential equations.

     xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;


 ** _CoqProject config file contents

   -R ~/opt/lib/compcert/coq compcert -R . Top

 *)

Require Import DigUty.float_text_io.
From compcert Require Import Floats.
Import Float.
From compcert Require Import Fappli_IEEE.
From compcert Require Import Integers.
Require Import Coq.Lists.List.
Require Import BinNums.
Require Import Coq.QArith.QArith_base.
Require Import Strings.String.
Require Import Strings.Ascii.
Require Import Coq.ZArith.Znat.
Require Import Recdef.

Import FloatIO.


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
