Require Export Task.driver_requires.
Import ListNotations.
Import FloatIO.
Import DScopeNotations.
Open Scope D_scope.

(* Inductive type used to name state variables. *)
Inductive stateVar : Set := 
| SvT
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT
(* Kinematics *)
| SvVELOCITY
| SvTHETA_B
| SvX_BI_I
| SvZ_BI_I
| SvXD_BI_I
| SvZD_BI_I
(* Seeker *)
| SvQ_S
| SvQ_S_MEAS
(* Flight computer *)
| SvGUIDANCE_GAIN
| SvTHETA_DOT_B_CMD
(* Airframe response *)
| SvTHETA_DOT_B.

Lemma state_var_eq: forall (r1 r2: stateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

Definition svIndex (Sv: stateVar) : positive :=
  match Sv with
  | SvT => 1
  | SvT_STOP => 2
  | SvDT => 3
  | SvDT_MAX => 4
  | SvDT_MIN => 5
  | SvDT_PRINT => 6
  | SvVELOCITY => 7
  | SvTHETA_B => 8
  | SvX_BI_I => 9
  | SvZ_BI_I => 10
  | SvXD_BI_I => 11
  | SvZD_BI_I => 12
  | SvQ_S => 13
  | SvQ_S_MEAS => 14
  | SvGUIDANCE_GAIN => 15
  | SvTHETA_DOT_B_CMD => 16
  | SvTHETA_DOT_B => 17
  end.

Definition svStrList :=
  [
    (SvT, "T");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT");
    (SvVELOCITY, "VELOCITY");
    (SvTHETA_B, "THETA_B");
    (SvX_BI_I, "X_BI_I");
    (SvZ_BI_I, "Z_BI_I");
    (SvXD_BI_I, "XD_BI_I");
    (SvZD_BI_I, "ZD_BI_I");
    (SvQ_S, "Q_S");
    (SvQ_S_MEAS, "Q_S_MEAS");
    (SvGUIDANCE_GAIN, "GUIDANCE_GAIN");
    (SvTHETA_DOT_B_CMD, "THETA_DOT_B_CMD");
    (SvTHETA_DOT_B, "THETA_DOT_B")
  ].

(* This has same keys as driver defaults, but has model over-rides *)
Definition model_driver_defaults_values :=
  [
            (SvT,        "0.0"#D);
            (SvT_STOP,   "10.0"#D);
            (SvDT,       "0.005"#D);
            (SvDT_MAX,   "0.005"#D);
            (SvDT_MIN,   "0.005"#D);
            (SvDT_PRINT, "0.01"#D)
  ].

Definition kinematics_default_data :=
  [
            (SvVELOCITY, "100.0"#D);
            (SvTHETA_B,  "0.0"#D);
            (SvX_BI_I,   "-500.0"#D);
            (SvZ_BI_I,   "-100.0"#D)
  ].

Definition seeker_default_data : list (stateVar * float) := [].

Definition flight_computer_default_data :=
  [
            (SvGUIDANCE_GAIN, "3.0"#D)
  ].

Definition airframe_response_default_data : list (stateVar * float) := [].

Definition model_default_values (_: unit) :=
  model_driver_defaults_values ++
  kinematics_default_data ++
  seeker_default_data ++
  flight_computer_default_data ++
  airframe_response_default_data.

Definition modelOutputs : list stateVar :=
  [SvT; SvX_BI_I; SvZ_BI_I; SvXD_BI_I; SvZD_BI_I; SvTHETA_B; SvQ_S].

Definition modelPairs : list (stateVar * stateVar) :=
  [(SvX_BI_I,  SvXD_BI_I); (SvZ_BI_I,  SvZD_BI_I); (SvTHETA_B, SvTHETA_DOT_B)].
