Require Export Task.driver_requires.
Import ListNotations.
Import FloatIO.
Open Scope float.


(* Inductive type used to name state variables. *)
Inductive stateVar : Set :=
(* Driver common *)
| SvT
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT
(* Model specific *)
| SvACC_GRAVITY
| SvAIR_DENSITY
| SvALPHA
| SvALPHA_TOTAL
| SvBETA
| SvCMP
| SvCMQ
| SvCMR
| SvCMX_PER_DELP
| SvCMY_PER_ALPHA
| SvCMY_PER_DELQ
| SvCMZ_PER_BETA
| SvCMZ_PER_DELR
| SvCX_BASE
| SvCX_PER_ALPHA_TOTAL
| SvCY_PER_BETA
| SvCZ_PER_ALPHA
| SvDEL_CMD_PER_P_CMD
| SvDEL_CMD_PER_Q_CMD
| SvDEL_CMD_PER_R_CMD
| SvAAEROX_BI_B
| SvAAEROY_BI_B
| SvAAEROZ_BI_B
| SvFIN_1_CMD
| SvFIN_1_POSITION
| SvFIN_2_CMD
| SvFIN_2_POSITION
| SvFIN_3_CMD
| SvFIN_3_POSITION
| SvFIN_4_CMD
| SvFIN_4_POSITION
| SvFIN_LIMIT
| SvIXX_B
| SvIYY_B
| SvIZZ_B
| SvMAEROX_BI_B
| SvMAEROY_BI_B
| SvMAEROZ_BI_B
| SvP_B
| SvP_B_CMD
| SvP_B_IC_DG
| SvPD_B
| SvP_G_MEAS
| SvPHI_B
| SvPHI_B_EST
| SvPHI_B_EST_DG
| SvPHI_B_IC_DG
| SvPITCH_GUIDANCE_GAIN
| SvP_S
| SvPSI_B
| SvPSI_B_EST
| SvPSI_B_EST_DG
| SvPSI_B_IC_DG
| SvQ0_B
| SvQ0_B_EST
| SvQ0D_B
| SvQ0D_B_EST
| SvQ1_B
| SvQ1_B_EST
| SvQ1D_B
| SvQ1D_B_EST
| SvQ2_B
| SvQ2_B_EST
| SvQ2D_B
| SvQ2D_B_EST
| SvQ3_B
| SvQ3_B_EST
| SvQ3D_B
| SvQ3D_B_EST
| SvQ_B
| SvQ_B_CMD
| SvQ_B_CMD_BIAS
| SvQ_B_IC_DG
| SvQD_B
| SvQ_G_MEAS
| SvQ_S
| SvQ_SI_B_MEAS
| SvRANGE_TB_SQ
| SvR_B
| SvR_B_CMD
| SvR_B_IC_DG
| SvRD_B
| SvREF_AREA
| SvREF_LENGTH
| SvR_G_MEAS
| SvROLL_GUIDANCE_GAIN
| SvR_S | SvR_SI_B_MEAS
| SvTHETA_B | SvTHETA_B_EST
| SvTHETA_B_EST_DG
| SvTHETA_B_IC_DG
| SvTIB11
| SvTIB12
| SvTIB13
| SvTIB21
| SvTIB22
| SvTIB23
| SvTIB31
| SvTIB32
| SvTIB33
| SvX_BI_I
| SvX_BI_I_IC
| SvXD_BI_I
| SvXD_BI_I_IC
| SvXDD_BI_I
| SvXD_TB_I
| SvXD_TI_I
| SvXD_TI_I_IC
| SvX_TB_I
| SvX_TI_I
| SvX_TI_I_IC
| SvYAW_GUIDANCE_GAIN
| SvY_BI_I
| SvY_BI_I_IC
| SvYD_BI_I
| SvYD_BI_I_IC
| SvYDD_BI_I
| SvYD_TB_I
| SvYD_TI_I
| SvYD_TI_I_IC
| SvY_TB_I
| SvY_TI_I
| SvY_TI_I_IC
| SvZ_BI_I
| SvZ_BI_I_IC
| SvZD_BI_I
| SvZD_BI_I_IC
| SvZDD_BI_I
| SvZD_TB_I
| SvZD_TI_I
| SvZD_TI_I_IC
| SvZ_TB_I
| SvZ_TI_I
| SvZ_TI_I_IC
| SvVELSQ
| SvDRAG_PER_VELSQ
| SvOMEGA0_q
| SvOMEGA0_r
| SvZETA_Q
| SvRMIN_XZ
| SvALPHA_REF
| SvVMAG
| SvZ_ACC_MAX
.

Lemma state_var_eq: forall (r1 r2: stateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

Definition svIndex (Sv: stateVar) : positive :=
  match Sv with
  | SvT => 1   | SvT_STOP => 2   | SvDT => 3   | SvDT_MAX => 4
  | SvDT_MIN => 5   | SvDT_PRINT => 6   | SvACC_GRAVITY => 7   | SvAIR_DENSITY => 8
  | SvALPHA => 9   | SvALPHA_TOTAL => 10   | SvBETA => 11   | SvCMP => 12
  | SvCMQ => 13   | SvCMR => 14   | SvCMX_PER_DELP => 15   | SvCMY_PER_ALPHA => 16
  | SvCMY_PER_DELQ => 17   | SvCMZ_PER_BETA => 18   | SvCMZ_PER_DELR => 19   | SvCX_BASE => 20
  | SvCX_PER_ALPHA_TOTAL => 21   | SvCY_PER_BETA => 22   | SvCZ_PER_ALPHA => 23   | SvDEL_CMD_PER_P_CMD => 24
  | SvDEL_CMD_PER_Q_CMD => 25   | SvDEL_CMD_PER_R_CMD => 26   | SvAAEROX_BI_B => 27   | SvAAEROY_BI_B => 28
  | SvAAEROZ_BI_B => 29   | SvFIN_1_CMD => 30   | SvFIN_1_POSITION => 31   | SvFIN_2_CMD => 32
  | SvFIN_2_POSITION => 33   | SvFIN_3_CMD => 34   | SvFIN_3_POSITION => 35   | SvFIN_4_CMD => 36
  | SvFIN_4_POSITION => 37   | SvFIN_LIMIT => 38   | SvIXX_B => 39   | SvIYY_B => 40
  | SvIZZ_B => 41   | SvMAEROX_BI_B => 42   | SvMAEROY_BI_B => 43   | SvMAEROZ_BI_B => 44
  | SvP_B => 46   | SvP_B_CMD => 47   | SvP_B_IC_DG => 48
  | SvPD_B => 49   | SvP_G_MEAS => 50   | SvPHI_B => 51   | SvPHI_B_EST => 52
  | SvPHI_B_EST_DG => 53   | SvPHI_B_IC_DG => 54   | SvPITCH_GUIDANCE_GAIN => 55   | SvP_S => 56
  | SvPSI_B => 57   | SvPSI_B_EST => 58   | SvPSI_B_EST_DG => 59   | SvPSI_B_IC_DG => 60
  | SvQ0_B => 61   | SvQ0_B_EST => 62   | SvQ0D_B => 63   | SvQ0D_B_EST => 64
  | SvQ1_B => 65   | SvQ1_B_EST => 66   | SvQ1D_B => 67   | SvQ1D_B_EST => 68
  | SvQ2_B => 69   | SvQ2_B_EST => 70   | SvQ2D_B => 71   | SvQ2D_B_EST => 72
  | SvQ3_B => 73   | SvQ3_B_EST => 74   | SvQ3D_B => 75   | SvQ3D_B_EST => 76
  | SvQ_B => 77   | SvQ_B_CMD => 78   | SvQ_B_CMD_BIAS => 79   | SvQ_B_IC_DG => 80
  | SvQD_B => 81   | SvQ_G_MEAS => 82   | SvQ_S => 83   | SvQ_SI_B_MEAS => 84
  | SvRANGE_TB_SQ => 85   | SvR_B => 86   | SvR_B_CMD => 87   | SvR_B_IC_DG => 88
  | SvRD_B => 89   | SvREF_AREA => 90   | SvREF_LENGTH => 91   | SvR_G_MEAS => 92
  | SvROLL_GUIDANCE_GAIN => 93   | SvR_S => 94   | SvR_SI_B_MEAS => 95   | SvTHETA_B => 96
  | SvTHETA_B_EST => 97   | SvTHETA_B_EST_DG => 98   | SvTHETA_B_IC_DG => 99   | SvTIB11 => 100
  | SvTIB12 => 101   | SvTIB13 => 102   | SvTIB21 => 103   | SvTIB22 => 104
  | SvTIB23 => 105   | SvTIB31 => 106   | SvTIB32 => 107   | SvTIB33 => 108
  | SvX_BI_I => 109   | SvX_BI_I_IC => 110   | SvXD_BI_I => 111   | SvXD_BI_I_IC => 112
  | SvXDD_BI_I => 113   | SvXD_TB_I => 114   | SvXD_TI_I => 115   | SvXD_TI_I_IC => 116
  | SvX_TB_I => 117   | SvX_TI_I => 118   | SvX_TI_I_IC => 119   | SvYAW_GUIDANCE_GAIN => 120
  | SvY_BI_I => 121   | SvY_BI_I_IC => 122   | SvYD_BI_I => 123   | SvYD_BI_I_IC => 124
  | SvYDD_BI_I => 125   | SvYD_TB_I => 126   | SvYD_TI_I => 127   | SvYD_TI_I_IC => 128
  | SvY_TB_I => 129   | SvY_TI_I => 130   | SvY_TI_I_IC => 131   | SvZ_BI_I => 132
  | SvZ_BI_I_IC => 133   | SvZD_BI_I => 134   | SvZD_BI_I_IC => 135   | SvZDD_BI_I => 136
  | SvZD_TB_I => 137   | SvZD_TI_I => 138   | SvZD_TI_I_IC => 139   | SvZ_TB_I => 140
  | SvZ_TI_I => 141   | SvZ_TI_I_IC => 142 | SvVELSQ => 143 | SvDRAG_PER_VELSQ => 144
  | SvOMEGA0_q => 145 | SvOMEGA0_r => 146  | SvZETA_Q => 147 | SvRMIN_XZ => 148
  | SvALPHA_REF => 149 | SvVMAG => 150 | SvZ_ACC_MAX => 151
  end.

Definition svStrList :=
  [
    (SvT, "T");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT");
    (SvACC_GRAVITY, "ACC_GRAVITY");
    (SvAIR_DENSITY, "AIR_DENSITY");
    (SvALPHA, "ALPHA");
    (SvALPHA_TOTAL, "ALPHA_TOTAL");
    (SvBETA, "BETA");
    (SvCMP, "CMP");
    (SvCMQ, "CMQ");
    (SvCMR, "CMR");
    (SvCMX_PER_DELP, "CMX_PER_DELP");
    (SvCMY_PER_ALPHA, "CMY_PER_ALPHA");
    (SvCMY_PER_DELQ, "CMY_PER_DELQ");
    (SvCMZ_PER_BETA, "CMZ_PER_BETA");
    (SvCMZ_PER_DELR, "CMZ_PER_DELR");
    (SvCX_BASE, "CX_BASE");
    (SvCX_PER_ALPHA_TOTAL, "CX_PER_ALPHA_TOTAL");
    (SvCY_PER_BETA, "CY_PER_BETA");
    (SvCZ_PER_ALPHA, "CZ_PER_ALPHA");
    (SvDEL_CMD_PER_P_CMD, "DEL_CMD_PER_P_CMD");
    (SvDEL_CMD_PER_Q_CMD, "DEL_CMD_PER_Q_CMD");
    (SvDEL_CMD_PER_R_CMD, "DEL_CMD_PER_R_CMD");
    (SvAAEROX_BI_B, "AAEROX_BI_B");
    (SvAAEROY_BI_B, "AAEROY_BI_B");
    (SvAAEROZ_BI_B, "AAEROZ_BI_B");
    (SvFIN_1_CMD, "FIN_1_CMD");
    (SvFIN_1_POSITION, "FIN_1_POSITION");
    (SvFIN_2_CMD, "FIN_2_CMD");
    (SvFIN_2_POSITION, "FIN_2_POSITION");
    (SvFIN_3_CMD, "FIN_3_CMD");
    (SvFIN_3_POSITION, "FIN_3_POSITION");
    (SvFIN_4_CMD, "FIN_4_CMD");
    (SvFIN_4_POSITION, "FIN_4_POSITION");
    (SvFIN_LIMIT, "FIN_LIMIT");
    (SvIXX_B, "IXX_B");
    (SvIYY_B, "IYY_B");
    (SvIZZ_B, "IZZ_B");
    (SvMAEROX_BI_B, "MAEROX_BI_B");
    (SvMAEROY_BI_B, "MAEROY_BI_B");
    (SvMAEROZ_BI_B, "MAEROZ_BI_B");
    (SvP_B, "P_B");
    (SvP_B_CMD, "P_B_CMD");
    (SvP_B_IC_DG, "P_B_IC_DG");
    (SvPD_B, "PD_B");
    (SvP_G_MEAS, "P_G_MEAS");
    (SvPHI_B, "PHI_B");
    (SvPHI_B_EST, "PHI_B_EST");
    (SvPHI_B_EST_DG, "PHI_B_EST_DG");
    (SvPHI_B_IC_DG, "PHI_B_IC_DG");
    (SvPITCH_GUIDANCE_GAIN, "PITCH_GUIDANCE_GAIN");
    (SvP_S, "P_S");
    (SvPSI_B, "PSI_B");
    (SvPSI_B_EST, "PSI_B_EST");
    (SvPSI_B_EST_DG, "PSI_B_EST_DG");
    (SvPSI_B_IC_DG, "PSI_B_IC_DG");
    (SvQ0_B, "Q0_B");
    (SvQ0_B_EST, "Q0_B_EST");
    (SvQ0D_B, "Q0D_B");
    (SvQ0D_B_EST, "Q0D_B_EST");
    (SvQ1_B, "Q1_B");
    (SvQ1_B_EST, "Q1_B_EST");
    (SvQ1D_B, "Q1D_B");
    (SvQ1D_B_EST, "Q1D_B_EST");
    (SvQ2_B, "Q2_B");
    (SvQ2_B_EST, "Q2_B_EST");
    (SvQ2D_B, "Q2D_B");
    (SvQ2D_B_EST, "Q2D_B_EST");
    (SvQ3_B, "Q3_B");
    (SvQ3_B_EST, "Q3_B_EST");
    (SvQ3D_B, "Q3D_B");
    (SvQ3D_B_EST, "Q3D_B_EST");
    (SvQ_B, "Q_B");
    (SvQ_B_CMD, "Q_B_CMD");
    (SvQ_B_CMD_BIAS, "Q_B_CMD_BIAS");
    (SvQ_B_IC_DG, "Q_B_IC_DG");
    (SvQD_B, "QD_B");
    (SvQ_G_MEAS, "Q_G_MEAS");
    (SvQ_S, "Q_S");
    (SvQ_SI_B_MEAS, "Q_SI_B_MEAS");
    (SvRANGE_TB_SQ, "RANGE_TB_SQ");
    (SvR_B, "R_B");
    (SvR_B_CMD, "R_B_CMD");
    (SvR_B_IC_DG, "R_B_IC_DG");
    (SvRD_B, "RD_B");
    (SvREF_AREA, "REF_AREA");
    (SvREF_LENGTH, "REF_LENGTH");
    (SvR_G_MEAS, "R_G_MEAS");
    (SvROLL_GUIDANCE_GAIN, "ROLL_GUIDANCE_GAIN");
    (SvR_S, "R_S");
    (SvR_SI_B_MEAS, "R_SI_B_MEAS");
    (SvTHETA_B, "THETA_B");
    (SvTHETA_B_EST, "THETA_B_EST");
    (SvTHETA_B_EST_DG, "THETA_B_EST_DG");
    (SvTHETA_B_IC_DG, "THETA_B_IC_DG");
    (SvTIB11, "TIB11");
    (SvTIB12, "TIB12");
    (SvTIB13, "TIB13");
    (SvTIB21, "TIB21");
    (SvTIB22, "TIB22");
    (SvTIB23, "TIB23");
    (SvTIB31, "TIB31");
    (SvTIB32, "TIB32");
    (SvTIB33, "TIB33");
    (SvX_BI_I, "X_BI_I");
    (SvX_BI_I_IC, "X_BI_I_IC");
    (SvXD_BI_I, "XD_BI_I");
    (SvXD_BI_I_IC, "XD_BI_I_IC");
    (SvXDD_BI_I, "XDD_BI_I");
    (SvXD_TB_I, "XD_TB_I");
    (SvXD_TI_I, "XD_TI_I");
    (SvXD_TI_I_IC, "XD_TI_I_IC");
    (SvX_TB_I, "X_TB_I");
    (SvX_TI_I, "X_TI_I");
    (SvX_TI_I_IC, "X_TI_I_IC");
    (SvYAW_GUIDANCE_GAIN, "YAW_GUIDANCE_GAIN");
    (SvY_BI_I, "Y_BI_I");
    (SvY_BI_I_IC, "Y_BI_I_IC");
    (SvYD_BI_I, "YD_BI_I");
    (SvYD_BI_I_IC, "YD_BI_I_IC");
    (SvYDD_BI_I, "YDD_BI_I");
    (SvYD_TB_I, "YD_TB_I");
    (SvYD_TI_I, "YD_TI_I");
    (SvYD_TI_I_IC, "YD_TI_I_IC");
    (SvY_TB_I, "Y_TB_I");
    (SvY_TI_I, "Y_TI_I");
    (SvY_TI_I_IC, "Y_TI_I_IC");
    (SvZ_BI_I, "Z_BI_I");
    (SvZ_BI_I_IC, "Z_BI_I_IC");
    (SvZD_BI_I, "ZD_BI_I");
    (SvZD_BI_I_IC, "ZD_BI_I_IC");
    (SvZDD_BI_I, "ZDD_BI_I");
    (SvZD_TB_I, "ZD_TB_I");
    (SvZD_TI_I, "ZD_TI_I");
    (SvZD_TI_I_IC, "ZD_TI_I_IC");
    (SvZ_TB_I, "Z_TB_I");
    (SvZ_TI_I, "Z_TI_I");
    (SvZ_TI_I_IC, "Z_TI_I_IC");
    (SvVELSQ, "VELOCITY_SQUARED");
    (SvDRAG_PER_VELSQ, "DRAG_PER_VELOCITY_SQUARED");
    (SvOMEGA0_q, "OMEGA_Q");
    (SvOMEGA0_r, "OMEGA_R");
    (SvZETA_Q,  "ZETA_Q");
    (SvRMIN_XZ, "RMIN_XZ");
    (SvALPHA_REF, "ALPHA_REF");
    (SvVMAG, "VELOCITY MAGNITUDE");
    (SvZ_ACC_MAX, "MAXIMUM Z ACCELERATION")
  ].

(* This has same keys as driver defaults, but has model over-rides *)
Definition model_driver_defaults_values :=
  [
            (SvT,        0.0);
            (SvT_STOP,   10.0);
            (SvDT,       0.005);
            (SvDT_MAX,   0.005);
            (SvDT_MIN,   0.001);
            (SvDT_PRINT, 0.005)
  ].

Definition rd_to_dg : float := 180.0/pi.

Definition kinematics_default_data : list (stateVar * float) :=
  [
            (SvPSI_B_IC_DG,   0.0);
            (SvTHETA_B_IC_DG, 0.0);
            (SvPHI_B_IC_DG,   0.0);
            (SvP_B_IC_DG,     0.0);
            (SvQ_B_IC_DG,     0.0);
            (SvR_B_IC_DG,     0.0);
            (SvXD_BI_I_IC,    100.0);
            (SvYD_BI_I_IC,    0.0);
            (SvZD_BI_I_IC,    0.0);
            (SvX_BI_I_IC,     0.0);
            (SvY_BI_I_IC,     0.0);
            (SvZ_BI_I_IC,     -100.0);
            (SvACC_GRAVITY,   9.88)
  ].

Definition gyro_default_data : list (stateVar * float) :=
  [
  ].

Definition target_default_data : list (stateVar * float) :=
  [
            (SvX_TI_I_IC,  500.0);
            (SvY_TI_I_IC,  -250.0);
            (SvZ_TI_I_IC,  0.0);
            (SvXD_TI_I_IC, -25.0);
            (SvYD_TI_I_IC, 25.0);
            (SvZD_TI_I_IC, 0.0)
  ].

Definition seeker_default_data : list (stateVar * float) :=
  [
  ].

Definition flight_computer_default_data : list (stateVar * float) :=
  [
            (SvROLL_GUIDANCE_GAIN,  -20.0);
            (SvPITCH_GUIDANCE_GAIN, 4.0);
            (SvYAW_GUIDANCE_GAIN,   4.0);
            (SvDEL_CMD_PER_P_CMD,   0.01);
            (SvDEL_CMD_PER_Q_CMD,   1.0);
            (SvDEL_CMD_PER_R_CMD,   1.0);
            (SvPSI_B_EST_DG,        0.0);
            (SvTHETA_B_EST_DG,      0.0);
            (SvPHI_B_EST_DG,        0.0);
            (SvQ_B_CMD_BIAS,        0.0988)
  ].

Definition airframe_response_default_data : list (stateVar * float) :=
  [
      (SvDRAG_PER_VELSQ, 0.00021);
      (SvOMEGA0_q, 30.0);
      (SvOMEGA0_r, 0.0);
      (SvZETA_Q,  0.5);
      (SvRMIN_XZ, 200.0);
      (SvALPHA_REF, 0.2618)
  ].

Definition model_default_values (_: unit) :=
  model_driver_defaults_values ++
  kinematics_default_data ++
  gyro_default_data ++
  target_default_data ++
  seeker_default_data ++
  flight_computer_default_data ++
  airframe_response_default_data.

Definition modelOutputs : list stateVar :=
  [ SvT; SvX_BI_I; SvY_BI_I; SvZ_BI_I; SvX_TI_I; SvY_TI_I; SvZ_TI_I; SvXD_BI_I;
    SvYD_BI_I; SvZD_BI_I; SvPSI_B; SvTHETA_B; SvPHI_B; SvP_B; SvQ_B; SvR_B;
    SvQ_SI_B_MEAS; SvR_SI_B_MEAS; SvPSI_B_EST; SvTHETA_B_EST; SvPHI_B_EST;
    SvP_B_CMD; SvQ_B_CMD; SvR_B_CMD
  ].

Definition modelPairs : list (stateVar * stateVar) :=
  [
            (SvQ0_B_EST, SvQ0D_B_EST);
            (SvQ1_B_EST, SvQ1D_B_EST);
            (SvQ2_B_EST, SvQ2D_B_EST);
            (SvQ3_B_EST, SvQ3D_B_EST);
            (SvX_BI_I,   SvXD_BI_I);
            (SvXD_BI_I,  SvXDD_BI_I);
            (SvY_BI_I,   SvYD_BI_I);
            (SvYD_BI_I,  SvYDD_BI_I);
            (SvZ_BI_I,   SvZD_BI_I);
            (SvZD_BI_I,  SvZDD_BI_I);
            (SvP_B,      SvPD_B);
            (SvQ_B,      SvQD_B);
            (SvR_B,      SvRD_B);
            (SvQ0_B,     SvQ0D_B);
            (SvQ1_B,     SvQ1D_B);
            (SvQ2_B,     SvQ2D_B);
            (SvQ3_B,     SvQ3D_B)
  ].
