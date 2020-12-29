(*+ Second Order Differential Equation Simulation *)

Require Import ZArith.
Require Import Coq.Floats.Floats.
Require Export Coq.Lists.List.
Import ListNotations.
Require Import Strings.String.
Require Import Task.float_text_io.
Import FloatIO.
Open Scope float.
Require Import Task.debug_printers.
Import DebugIO.

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

(* The state that changes with each step *)
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

(* The intial state *)
Definition st0 := {| time := time0; x := x_ic; xd := xd_ic |}.

(* Performs one step of the simulation.
   Input is a list of states already reached,
   with the most recent at the front.
   It computes the next state and adds it to
   the front of the list. If the input list
   is empty, we add the initial state.
*)
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

(* Compute the number of iteration steps needed *)
Definition z_steps :=
  Eval native_compute in 
  let steps := (tstop - time0) / dt in
  ZofFloat steps.

(* Computes the list of states, reversing so the first state is first *)
Definition stfinal := Eval native_compute in rev (Z.iter z_steps oneStep [st0]).

(* Format a list of states into lines of text. *)
Fixpoint formatStates (states : list State) :=
  match states with
  | nil => ""
  | cons {| time := t1; x:= x1; xd := xd1 |} others =>
    (print_float t1) ++ (print_float x1) ++ (print_float xd1) ++ nl ++
                     (formatStates others)
  end.

(* This outputs the data, with markers so we can extract the data from
   other Coq output that we do not care about (such as the type of the output.
*)
Compute nl ++ "========== Start of data" ++ nl ++ (formatStates stfinal)
++ "========== End of data" ++ nl.