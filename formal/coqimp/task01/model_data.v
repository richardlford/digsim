Require Export Task.driver1.
Import ListNotations.

  
(* Inductive type used to name state variables. *)
Inductive stateVar : Set := 
| SvT
| SvX
| SvXD
| SvXDD
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT
| SvDampingCoefficient
| SvSpringCoefficient
| SvGRAVITY
| SvMass.

Lemma state_var_eq: forall (r1 r2: stateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

Definition svIndex (sv: stateVar) : positive :=
  match sv with
  | SvT => 1
  | SvX => 2
  | SvXD => 3
  | SvXDD => 4
  | SvT_STOP => 5
  | SvDT => 6
  | SvDT_MAX => 7
  | SvDT_MIN => 8
  | SvDT_PRINT => 9
  | SvDampingCoefficient => 10
  | SvSpringCoefficient => 11
  | SvGRAVITY => 12
  | SvMass => 13
  end.


Definition svStrList :=
  [
    (SvT, "T");
    (SvX, "X");
    (SvXD, "XD");
    (SvXDD, "XDD");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT");
    (SvDampingCoefficient, "SvDampingCoefficient");
    (SvSpringCoefficient, "SvSpringCoefficient");
    (SvGRAVITY, "GRAVITY");
    (SvMass, "Mass")
  ].

Definition driver_defaults_str :=
  [
            (SvT,        "0.0");
            (SvT_STOP,   "0.0");
            (SvDT,       "0.005");
            (SvDT_MAX,   "0.005");
            (SvDT_MIN,   "0.005");
            (SvDT_PRINT, "0.01")
  ].

Definition model_default_values_str :=
  [
    (SvT,             "0.0");
    (SvX,             "0.0");
    (SvXD,            "0.0");
    (SvXDD,           "0.0");
    (SvT_STOP,        "2.5");
    (SvDT,            "0.01");
    (SvDT_MAX,        "0.005");
    (SvDT_MIN,        "0.001");
    (SvDT_PRINT,      "0.01");
    (SvDampingCoefficient, "8.88");
    (SvSpringCoefficient, "39.47");
    (SvGRAVITY,       "9.88");
    (SvMass,          "1.0")
  ].

Definition modelOutputs : list stateVar := [SvT; SvX; SvXD].
