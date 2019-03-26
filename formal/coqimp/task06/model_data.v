Require Export Task.driver1.
Import ListNotations.

  
(* Inductive type used to name state variables. *)
Inductive stateVar : Set := 
| SvT
| SvX
| SvXD
| SvXDD
| SvZ
| SvZD
| SvTHETA
| SvTHETA_DOT
| SvTHETA_DOT_CMD
| SvQ_S
| SvQ_S_MEAS
| SvVELOCITY
| SvGUIDANCE_GAIN
| SvTHETA_IC_DG
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT.

Lemma state_var_eq: forall (r1 r2: stateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.


Definition svIndex (Sv: stateVar) : positive :=
  match Sv with
  | SvT => 1
  | SvX => 2
  | SvXD => 3
  | SvXDD => 4
  | SvZ => 5
  | SvZD => 6
  | SvTHETA => 7
  | SvTHETA_DOT => 8
  | SvTHETA_DOT_CMD => 9
  | SvQ_S => 10
  | SvQ_S_MEAS => 11
  | SvVELOCITY => 12
  | SvGUIDANCE_GAIN => 13
  | SvTHETA_IC_DG => 14
  | SvT_STOP => 15
  | SvDT => 16
  | SvDT_MAX => 17
  | SvDT_MIN => 18
  | SvDT_PRINT => 19
  end.


Definition svStrList :=
  [
    (SvT, "T");
    (SvX, "X");
    (SvXD, "XD");
    (SvXDD, "XDD");
    (SvZ, "Z");
    (SvZD, "ZD");
    (SvTHETA, "THETA");
    (SvTHETA_DOT, "THETA_DOT");
    (SvTHETA_DOT_CMD, "THETA_DOT_CMD");
    (SvQ_S, "Q_S");
    (SvQ_S_MEAS, "Q_S_MEAS");
    (SvVELOCITY, "VELOCITY");
    (SvGUIDANCE_GAIN, "GUIDANCE_GAIN");
    (SvTHETA_IC_DG, "THETA_IC_DG");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT")
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
    (SvX,             "-500.0");
    (SvXD,            "0.0");
    (SvXDD,           "0.0");
    (SvZ,             "-100.0");
    (SvZD,            "0.0");
    (SvTHETA,         "0.0");
    (SvTHETA_DOT,     "0.0");
    (SvTHETA_DOT_CMD, "0.0");
    (SvQ_S,           "0.0");
    (SvQ_S_MEAS,      "0.0");
    (SvVELOCITY,      "100.0");
    (SvGUIDANCE_GAIN, "3.0");
    (SvTHETA_IC_DG,   "0.0");
    (SvT_STOP,        "10.0");
    (SvDT,            "0.005");
    (SvDT_MAX,        "0.005");
    (SvDT_MIN,        "0.005");
    (SvDT_PRINT,      "0.01")
  ].

Definition modelOutputs : list stateVar := [SvT; SvX; SvZ;SvTHETA; SvXD; SvZD; SvQ_S].
