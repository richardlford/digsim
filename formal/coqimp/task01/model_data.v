Require Export Task.driver_requires.
Import ListNotations.
Import FloatIO.
Import DScopeNotations.
Open Scope D_scope.
  
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

Definition model_default_values (_: unit) :=
  [
    (SvT,             "0.0"#D);
    (SvX,             "0.0"#D);
    (SvXD,            "0.0"#D);
    (SvXDD,           "0.0"#D);
    (SvT_STOP,        "2.5"#D);
    (SvDT,            "0.01"#D);
    (SvDT_MAX,        "0.005"#D);
    (SvDT_MIN,        "0.001"#D);
    (SvDT_PRINT,      "0.01"#D);
    (SvDampingCoefficient, "8.88"#D);
    (SvSpringCoefficient, "39.47"#D);
    (SvGRAVITY,       "9.88"#D);
    (SvMass,          "1.0"#D)
  ].

Definition modelOutputs : list stateVar := [SvT; SvX; SvXD].

Definition modelPairs : list (stateVar * stateVar) :=
  [(SvX, SvXD); (SvXD, SvXDD)].
