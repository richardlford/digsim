(*+ Second Order Differential Equation Simulation *)

Require Import ZArith.
Require Import Coq.Floats.Floats.
Open Scope float.
Require Export Coq.Lists.List.
Import ListNotations.

(* Define constants that do not vary with state *)

(* Simulation start time [sec] *)
Definition time0 : float := 0.0.

(* Simulation stop time [sec] *)
Definition tstop : float := 2.5.

(* Simulation time step [sec] *)
Definition dt : float := 0.01.

(* Damping force per velocity [N/m/s] *)
Definition damping_coefficient : float := 8.88.

(* Acceleration due to gravity [m/sec**2] *)
Definition gravity : float := 9.88.

(* Mass suspended from spring [Kg] *)
Definition mass : float := 1.0.

(* Restoring force per position [N/m] *)
Definition spring_coefficient : float := 39.47.

(* Initial position of suspended mass (m] *)
Definition xd_ic : float := 0.0.

(* Initial velocity of suspended mass [m/sec] *)
Definition x_ic : float := 0.0.

(* Formula for acceleration for given position and speed *)
Definition xdd_fun x xd :=
  (- (spring_coefficient * x + damping_coefficient * xd)) / mass - gravity.

(* The state the changes with each step *)
Record State : Type :=
  mkState
    {
      (* Simulation time [sec] *)
      time : float;

      (* Position of suspended mass (m] *)
      x : float;

      (* Velocity of suspended mass [m/sec] *)
      xd : float;
    }.

Definition st0 := {| time := time0; x := x_ic; xd := xd_ic |}.

Check list.
(* Compute st0. *)

Definition oneStep states :=
  match states with
  | nil => st0 :: nil
  | cons state x0 => 
    let xdd := xdd_fun state.(x) state.(xd) in
    let x1 := state.(x) + state.(xd) * dt in
    let xd1 := state.(xd) + xdd * dt in
    let t1 := state.(time) + dt in
    let state1 := {| time := t1; x:= x1; xd := xd1 |} in
    cons state1 states
  end.

Compute oneStep [st0].

Definition stfinal := Eval native_compute in rev (Z.iter 3%Z oneStep [st0]).
Print stfinal.
