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
| SvQ_B
| SvTHETA_B
| SvX_BI_I
| SvZ_BI_I
| SvXD_BI_I
| SvZD_BI_I
| SvXDD_BI_I
| SvZDD_BI_I
| SvGRAVITY
| SvALPHA_REF
| SvDRAG_PER_VELSQ
| SvOMEGA0_q
| SvQ_B_CMD
| SvRMIN_XZ
| SvZETA_Q
| SvAAEROX_BI_B
| SvAAEROZ_BI_B
| SvACC_ALPHA
| SvACC_PER_ALPHA
| SvAQI
| SvBQI
| SvQD_B
| SvVELSQ
| SvVMAG
| SvZ_ACC_MAX
(* Target *)
| SvX_TI_I
| SvXD_TI_I
| SvZ_TI_I
| SvZD_TI_I
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
  | SvQ_B  => 7
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
  | SvXDD_BI_I => 18
  | SvZDD_BI_I => 19
  | SvGRAVITY => 20
  | SvALPHA_REF => 21
  | SvDRAG_PER_VELSQ => 22
  | SvOMEGA0_q => 23
  | SvQ_B_CMD => 24
  | SvRMIN_XZ => 25
  | SvZETA_Q => 26
  | SvAAEROX_BI_B => 27
  | SvAAEROZ_BI_B => 28
  | SvACC_ALPHA => 29
  | SvACC_PER_ALPHA => 30
  | SvAQI => 31
  | SvBQI => 32
  | SvQD_B => 33
  | SvVELSQ => 34
  | SvVMAG => 35
  | SvZ_ACC_MAX => 36
  | SvX_TI_I => 37
  | SvXD_TI_I => 38
  | SvZ_TI_I => 39
  | SvZD_TI_I => 40
  end.
Arguments svIndex !Sv.

Definition svStrList :=
  [
    (SvT, "T");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT");
    (SvQ_B, "Q_B");
    (SvTHETA_B, "THETA_B");
    (SvX_BI_I, "X_BI_I");
    (SvZ_BI_I, "Z_BI_I");
    (SvXD_BI_I, "XD_BI_I");
    (SvZD_BI_I, "ZD_BI_I");
    (SvQ_S, "Q_S");
    (SvQ_S_MEAS, "Q_S_MEAS");
    (SvGUIDANCE_GAIN, "GUIDANCE_GAIN");
    (SvTHETA_DOT_B_CMD, "THETA_DOT_B_CMD");
    (SvTHETA_DOT_B, "THETA_DOT_B");
    (SvXDD_BI_I, "XDD_BI_I");
    (SvZDD_BI_I, "ZDD_BI_I");
    (SvGRAVITY, "Gravity");
    (SvALPHA_REF, "ALPHA_REF");
    (SvDRAG_PER_VELSQ, "DRAG_PER_VELOCITY_SQUARED");
    (SvOMEGA0_q, "OMEGAO_Q");
    (SvQ_B_CMD, "Q_B_CMD");
    (SvRMIN_XZ, "RMIN_XZ");
    (SvZETA_Q, "ZETA_Q");
    (SvAAEROX_BI_B, "AAEROX_BI_B");
    (SvAAEROZ_BI_B, "AAEROZ_BI_B");
    (SvACC_ALPHA, "ACC_ALPHA");
    (SvACC_PER_ALPHA, "ACC_PER_ALPHA");
    (SvAQI, "AQI");
    (SvBQI, "BQI");
    (SvQD_B, "QD_B");
    (SvVELSQ, "VELOCITY_SQUARED");
    (SvVMAG, "VMAG");
    (SvZ_ACC_MAX, "Z_ACC_MAX");
    (SvX_TI_I, "X_TI_I");
    (SvXD_TI_I, "XD_TI_I");
    (SvZ_TI_I, "Z_TI_I");
    (SvZD_TI_I, "ZD_TI_I")
  ].

(* This has same keys as driver defaults, but has model over-rides *)
Definition model_driver_defaults_values :=
  [
            (SvT,        "0.0"#D);
            (SvT_STOP,   "10.0"#D);
            (SvDT,       "0.1"#D);
            (SvDT_MAX,   "0.005"#D);
            (SvDT_MIN,   "0.001"#D);
            (SvDT_PRINT, "0.01"#D);
            (SvQ_S,  "0.0"#D);
            (SvQ_S_MEAS, "0.0"#D);
            (SvTHETA_DOT_B_CMD, "0.0"#D);
            (SvTHETA_DOT_B, "0.0"#D);
            (SvXDD_BI_I, "0.0"#D);
            (SvZDD_BI_I, "0.0"#D);
            (SvDRAG_PER_VELSQ, "0.0"#D);
            (SvQ_B_CMD, "0.0"#D);
            (SvAAEROX_BI_B, "0.0"#D);
            (SvAAEROZ_BI_B, "0.0"#D);
            (SvACC_ALPHA, "0.0"#D);
            (SvACC_PER_ALPHA, "0.0"#D);
            (SvAQI, "0.0"#D);
            (SvBQI, "0.0"#D);
            (SvQD_B, "0.0"#D);
            (SvVELSQ, "0.0"#D);
            (SvVMAG, "0.0"#D);
            (SvZ_ACC_MAX, "0.0"#D)
  ].

Definition kinematics_default_data :=
  [
            (SvTHETA_B,  "0.0"#D);
            (SvQ_B,      "0.0"#D);
            (SvXD_BI_I,  "100.0"#D);
            (SvZD_BI_I,  "0.0"#D);
            (SvX_BI_I,   "-500.0"#D);
            (SvZ_BI_I,   "-100.0"#D);
            (SvGRAVITY,  "9.88"#D)
  ].

Definition target_default_data : list (stateVar * float) :=
  [
            (SvXD_TI_I,  "0.0"#D);
            (SvZD_TI_I,  "0.0"#D);
            (SvX_TI_I,   "0.0"#D);
            (SvZ_TI_I,   "0.0"#D)
  ].

Definition seeker_default_data : list (stateVar * float) :=
  [

  ].

Definition flight_computer_default_data :=
  [
            (SvGUIDANCE_GAIN, "3.0"#D)
  ].

Definition airframe_response_default_data : list (stateVar * float) :=
  [
    (SvDRAG_PER_VELSQ, "0.00021"#D);
      (SvOMEGA0_q, "30.0"#D);
      (SvZETA_Q,  "0.5"#D);
      (SvRMIN_XZ, "200.0"#D);
      (SvALPHA_REF, "0.2618"#D)
  ].

Definition model_default_values (_: unit) :=
  model_driver_defaults_values ++
  kinematics_default_data ++
  target_default_data ++                             
  seeker_default_data ++
  flight_computer_default_data ++
  airframe_response_default_data.
Arguments model_default_values !u.

Definition modelOutputs : list stateVar :=
  [SvT; SvX_BI_I; SvZ_BI_I; SvXD_BI_I; SvZD_BI_I; SvX_TI_I; SvZ_TI_I; SvXD_TI_I; SvZD_TI_I; SvTHETA_B; SvQ_S].

Definition modelPairs : list (stateVar * stateVar) :=
  [(SvX_BI_I,  SvXD_BI_I); (SvZ_BI_I,  SvZD_BI_I); (SvXD_BI_I,  SvXDD_BI_I); (SvZD_BI_I,  SvZDD_BI_I); (SvX_TI_I,  SvXD_TI_I); (SvZ_TI_I, SvZD_TI_I); (SvTHETA_B, SvTHETA_DOT_B); (SvQ_B, SvQD_B)].
