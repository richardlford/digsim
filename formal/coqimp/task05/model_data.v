Require Export Task.driver1.
Import ListNotations.

  
(* Inductive type used to name state variables. *)
Inductive stateVar : Set := 
| SvT
| SvX
| SvXD
| SvXDD
| SvCOEFF_OF_REST
| SvGRAVITY
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT.

Lemma state_var_eq: forall (r1 r2: stateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

Definition svIndex (sv: stateVar) : positive :=
  match sv with
  | SvT => 1
  | SvX => 2
  | SvXD => 3
  | SvXDD => 4
  | SvCOEFF_OF_REST => 5
  | SvGRAVITY => 6
  | SvT_STOP => 7
  | SvDT => 8
  | SvDT_MAX => 9
  | SvDT_MIN => 10
  | SvDT_PRINT => 11
  end.


Definition svStrList :=
  [
    (SvT, "T");
    (SvX, "X");
    (SvXD, "XD");
    (SvXDD, "XDD");
    (SvCOEFF_OF_REST, "COEFF_OF_REST");
    (SvGRAVITY, "GRAVITY");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT")
  ]%positive.

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
    (SvX,             "10.0");
    (SvXD,            "0.0");
    (SvXDD,           "0.0");
    (SvCOEFF_OF_REST, "0.80");
    (SvGRAVITY,       "9.88");
    (SvT_STOP,        "10.0");
    (SvDT,            "0.01");
    (SvDT_MAX,        "0.005");
    (SvDT_MIN,        "0.001");
    (SvDT_PRINT,      "0.01")
  ].

Definition modelOutputs : list stateVar := [SvT; SvX; SvXD].

