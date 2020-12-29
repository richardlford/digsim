Require Export Task.driver_state.
Require Export Task.trig.
Import ListNotations.
Import FloatIO.
Import DebugIO.
Import RecordSetNotations.
Open Scope float.
Import CoqTrig.

Definition z_terminate_sim_event := {| key := "z_terminate_sim_event"; time := 0.0 |}.

Definition sq (f: float) := f * f.
Definition SMALL : float := 0.000001.

Definition log_miss (sim: simTy) (dt_miss: float) : simTy :=
  let svars := sim.(vars) in
  let t := svGetFloat SvT svars in
  let x_tb_i := svGetFloat SvX_TB_I svars in
  let y_tb_i := svGetFloat SvY_TB_I svars in
  let z_tb_i := svGetFloat SvZ_TB_I svars in
  let xd_tb_i := svGetFloat SvXD_TB_I svars in
  let yd_tb_i := svGetFloat SvYD_TB_I svars in
  let zd_tb_i := svGetFloat SvZD_TB_I svars in

  let r_miss := sqrt
                  (((x_tb_i - xd_tb_i * dt_miss) * (x_tb_i - xd_tb_i * dt_miss))
                   + ((y_tb_i - yd_tb_i * dt_miss) * (y_tb_i - yd_tb_i * dt_miss))
                   + ((z_tb_i - zd_tb_i * dt_miss) * (z_tb_i - zd_tb_i * dt_miss))
                  ) in
  let miss_vars :=
      [("t", print_float t);
       ("r_miss", print_float r_miss);
       ("dt_miss", print_float dt_miss)] in
  let miss_entry := {| le_caption := "log_miss"; le_vars := miss_vars; le_events := [] |} in
  sim<|log_entries ::= (fun oldlog => miss_entry :: oldlog)|>.

Definition z_terminate_sim_event_func  : event_function_signature :=
  fun (this: eventTy) (sim: simTy) =>
    let svars := sim.(vars) in
    let x_tb_i := svGetFloat SvX_TB_I svars in
    let y_tb_i := svGetFloat SvY_TB_I svars in
    let z_tb_i := svGetFloat SvZ_TB_I svars in
    let xd_tb_i := svGetFloat SvXD_TB_I svars in
    let yd_tb_i := svGetFloat SvYD_TB_I svars in
    let zd_tb_i := svGetFloat SvZD_TB_I svars in

    let time_to_go := -(x_tb_i * xd_tb_i + y_tb_i * yd_tb_i + z_tb_i * zd_tb_i) /
                        ((sq xd_tb_i) + (sq yd_tb_i) + (sq zd_tb_i)) in
    let result_sim :=
        if (time_to_go < 0.0) then
          let sim1 := log_miss sim time_to_go in
          let flags2 := sim1.(flags)<|end_of_run := true|> in
          sim1<|flags := flags2|>
        else
          sim
    in
    (result_sim, None).

Definition model_handlers :=
  ("z_terminate_sim_event", z_terminate_sim_event_func) ::
  driver_default_handlers.

(*+ Initial the modules *)
Definition airframe_response_init (sim: simTy) := sim.

Definition kinematics_init (sim: simTy) :=
  let svars := sim.(vars) in
  let psi_b := svGetFloat SvPSI_B_IC_DG svars / rd_to_dg in
  let theta_b := svGetFloat SvTHETA_B_IC_DG svars / rd_to_dg in
  let phi_b := svGetFloat SvPHI_B_IC_DG svars / rd_to_dg in
  let p_b := svGetFloat SvP_B_IC_DG svars / rd_to_dg in
  let q_b := svGetFloat SvQ_B_IC_DG svars / rd_to_dg in
  let r_b := svGetFloat SvR_B_IC_DG svars / rd_to_dg in

  let cpsio2 := cos(0.5 * psi_b) in
  let cthetao2 := cos(0.5 * theta_b) in
  let cphio2 := cos(0.5 * phi_b) in
  let spsio2 := sin(0.5 * psi_b) in
  let sthetao2 := sin(0.5 * theta_b) in
  let sphio2 := sin(0.5 * phi_b) in
  let q0_b := cpsio2 * cthetao2 * cphio2 + spsio2 * sthetao2 * sphio2 in
  let q1_b := cpsio2 * cthetao2 * sphio2 - spsio2 * sthetao2 * cphio2 in
  let q2_b := cpsio2 * sthetao2 * cphio2 + spsio2 * cthetao2 * sphio2 in
  let q3_b := spsio2 * cthetao2 * cphio2 - cpsio2 * sthetao2 * sphio2 in
  union_vars sim
             [
               (SvPSI_B, psi_b);
               (SvTHETA_B, theta_b);
               (SvPHI_B, phi_b);
               (SvP_B, p_b);
               (SvQ_B, q_b);
               (SvR_B, r_b);
               (SvQ0_B, q0_b);
               (SvQ1_B, q1_b);
               (SvQ2_B, q2_b);
               (SvQ3_B, q3_b);
               (SvX_BI_I, svGetFloat SvX_BI_I_IC svars);
               (SvY_BI_I, svGetFloat SvY_BI_I_IC svars);
               (SvZ_BI_I, svGetFloat SvZ_BI_I_IC svars);
               (SvXD_BI_I, svGetFloat SvXD_BI_I_IC svars);
               (SvYD_BI_I, svGetFloat SvYD_BI_I_IC svars);
               (SvZD_BI_I, svGetFloat SvZD_BI_I_IC svars)
             ].

Definition gyro_init (sim: simTy) := sim.

Definition target_init (sim: simTy) :=
  let svars := sim.(vars) in
  union_vars sim
             [
               (SvX_TI_I, svGetFloat SvX_TI_I_IC svars);
               (SvY_TI_I, svGetFloat SvY_TI_I_IC svars);
               (SvZ_TI_I, svGetFloat SvZ_TI_I_IC svars);
               (SvXD_TI_I, svGetFloat SvXD_TI_I_IC svars);
               (SvYD_TI_I, svGetFloat SvYD_TI_I_IC svars);
               (SvZD_TI_I, svGetFloat SvZD_TI_I_IC svars)
             ].

Definition seeker_init (sim: simTy) := sim.

Definition flight_computer_init (sim: simTy) :=
  let svars := sim.(vars) in
  let psi_b_est_dg := svGetFloat SvPSI_B_EST_DG svars in
  let theta_b_est_dg := svGetFloat SvTHETA_B_EST_DG svars in
  let phi_b_est_dg := svGetFloat SvPHI_B_EST_DG svars in

  (* Begin flight computer initialization: *)
  (* Initial flight computer states *)

  (* Trig functions of half Euler angles *)
  let cpsio2 := cos(0.5 * psi_b_est_dg / rd_to_dg) in
  let cthetao2 := cos(0.5 * theta_b_est_dg / rd_to_dg) in
  let cphio2 := cos(0.5 * phi_b_est_dg / rd_to_dg) in
  let spsio2 := sin(0.5 * psi_b_est_dg / rd_to_dg) in
  let sthetao2 := sin(0.5 * theta_b_est_dg / rd_to_dg) in
  let sphio2 := sin(0.5 * phi_b_est_dg / rd_to_dg) in

  (* Initial quaternion values *)
  let q0_b_est := cpsio2 * cthetao2 * cphio2 + spsio2 * sthetao2 * sphio2 in
  let q1_b_est := cpsio2 * cthetao2 * sphio2 + spsio2 * sthetao2 * cphio2 in
  let q2_b_est := cpsio2 * sthetao2 * cphio2 + spsio2 * cthetao2 * sphio2 in
  let q3_b_est := spsio2 * cthetao2 * cphio2 + cpsio2 * sthetao2 * sphio2 in

  union_vars sim
             [
               (SvQ0_B_EST, q0_b_est);
               (SvQ1_B_EST, q1_b_est);
               (SvQ2_B_EST, q2_b_est);
               (SvQ3_B_EST, q3_b_est)
             ].

Definition init_sim (sim0: simTy) :=
  let sim1 := kinematics_init sim0 in
  let sim2 := gyro_init sim1 in
  let sim3 := target_init sim2 in
  let sim4 := seeker_init sim3 in
  let sim5 := flight_computer_init sim4 in
  let sim6 := airframe_response_init sim5 in
  let sim7 := union_vars sim6
                         [
                           (SvROLL_GUIDANCE_GAIN, -20.0);
                           (SvPITCH_GUIDANCE_GAIN, 4.0);
                           (SvX_TI_I, 500.0);
                           (SvY_TI_I, -250.0)
                         ] in
  sim7<|sim_events ::= (fun others => z_terminate_sim_event::others)|>.


Definition limit (x l u  : float) : float :=
  if (x < l) then l
  else if (u < x) then u
  else  x.

Definition max (x y: float) : float :=
  if y < x then x else y.
Axiom SvBETA_REF : stateVar.
Axiom  SvOMEGA0_R : stateVar.
Axiom  SvR_B_SMD : stateVar.
Axiom Sv_RMIN_XY : stateVar.
Axiom SvTAU_P : stateVar.
Axiom SvZETA_R: stateVar.

Definition airframe_response (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  (*Inputs*)
  let Alpha_Ref := (svGetFloat SvALPHA_REF vars) in
  let Beta_Ref := (svGetFloat SvBETA_REF vars) in
  let Drag_Per_VelSq := (svGetFloat SvDRAG_PER_VELSQ vars) in
  let Omega0_q := (svGetFloat SvOMEGA0_q vars) in
  let Omega0_r := (svGetFloat SvOMEGA0_R vars) in
  let P_b := (svGetFloat SvP_B vars) in
  let Q_b := (svGetFloat SvQ_B vars) in
  let R_b := (svGetFloat SvR_B vars) in
  let P_b_Cmd := (svGetFloat SvP_B_CMD vars) in
  let Q_b_Cmd := (svGetFloat SvQ_B_CMD vars) in
  let R_b_Cmd := (svGetFloat SvR_B_SMD vars) in
  let Q0_b := (svGetFloat SvQ0_B vars) in
  let Q1_b := (svGetFloat SvQ1_B vars) in
  let Q2_b := (svGetFloat SvQ2_B vars) in
  let Q3_b := (svGetFloat SvQ3_B vars) in
  let Rmin_xy := (svGetFloat Sv_RMIN_XY vars) in
  let Rmin_xz := (svGetFloat SvRMIN_XZ vars) in
  let Tau_p := (svGetFloat SvTAU_P vars) in
  let Xd_bi_i := (svGetFloat SvXD_BI_I vars) in
  let Yd_bi_i := (svGetFloat SvYD_BI_I vars) in
  let Zd_bi_i := (svGetFloat SvZD_BI_I vars) in
  let Zeta_q := (svGetFloat SvZETA_Q vars) in
  let Zeta_r := (svGetFloat SvZETA_R vars) in
  (* Evaluate ICS to BCS transformation matrix *)
  let Tib11 := Q0_b*Q0_b + Q1_b*Q1_b - Q2_b*Q2_b - Q3_b*Q3_b in
  let Tib12 := 2.0*(Q1_b*Q2_b + Q0_b*Q3_b) in
  let  Tib13 := 2.0*(Q1_b*Q3_b - Q0_b*Q2_b) in
      let Tib21 := 2.0*(Q1_b*Q2_b - Q0_b*Q3_b) in
      let Tib22 := Q0_b*Q0_b + Q2_b*Q2_b - Q1_b*Q1_b - Q3_b*Q3_b in
      let Tib23 := 2.0*(Q2_b*Q3_b + Q0_b*Q1_b) in
      let Tib31 := 2.0*(Q1_b*Q3_b + Q0_b*Q2_b) in
      let Tib32 := 2.0*(Q2_b*Q3_b - Q0_b*Q1_b) in
      let Tib33 := Q0_b*Q0_b + Q3_b*Q3_b - Q1_b*Q1_b - Q2_b*Q2_b in
     (* Missile velocity WRT ICS origin in BCS *)
      let Xd_bi_b := Xd_bi_i*Tib11 + Yd_bi_i*Tib12 + Zd_bi_i*Tib13 in
      let Yd_bi_b := Xd_bi_i*Tib21 + Yd_bi_i*Tib22 + Zd_bi_i*Tib23 in
      let Zd_bi_b := Xd_bi_i*Tib31 + Yd_bi_i*Tib32 + Zd_bi_i*Tib33 in
      let VelSq   := Xd_bi_i*Xd_bi_i +
               Yd_bi_i*Yd_bi_i +
               Zd_bi_i*Zd_bi_i in
      let Vmag    := sqrt (VelSq) in
     (* Angle of attack and sideslip angle *)
      let Alpha           := atan2 Zd_bi_b Xd_bi_b in
      let Beta            := atan2 Yd_bi_b Xd_bi_b in
      let Beta_prime      := atan2 Yd_bi_b (sqrt (Xd_bi_b*Xd_bi_b + Zd_bi_b*Zd_bi_b)) in
      (* Aero angle trig functions *)
      let CAlpha := cos(Alpha) in
      let CBetap := cos(Beta_prime) in
      let SAlpha := sin(Alpha) in
      let SBetap := sin(Beta_prime) in
      (* Stability axis to body axis transformation matrix *)
      let Tstb11 :=  CAlpha*CBetap in
      let Tstb12 := -CAlpha*SBetap in
      let Tstb13 := -SAlpha in
      let Tstb21 :=  SBetap in
      let Tstb22 :=  CBetap in
      let Tstb23 :=  0.0 in
      let Tstb31 :=  SAlpha*CBetap in
      let Tstb32 := -SAlpha*SBetap in
      let Tstb33 :=  CAlpha in
      (* Translational accelerations due to aero in stability axis *)
      let Adrag           := -VelSq*Drag_Per_VelSq in
      let Z_Acc_Max       :=  VelSq/Rmin_xz in
      let Acc_Per_Alpha   := - (max SMALL (Z_Acc_Max/Alpha_Ref)) in
      let Acc_Alpha       :=  Acc_Per_Alpha*Alpha in
      let Y_Acc_Max       :=  VelSq/Rmin_xy in
      let Acc_Per_Beta    := - (max SMALL (Y_Acc_Max/Beta_Ref)) in
      let Acc_Beta        :=  Acc_Per_Beta*Beta in
      (* Translational acceleration due to aero in BCS *)
      let AaeroX_bi_b := Tstb11*Adrag + Tstb12*Acc_Beta + Tstb13*Acc_Alpha in
      let AaeroY_bi_b := Tstb21*Adrag + Tstb22*Acc_Beta + Tstb23*Acc_Alpha in
      let AaeroZ_bi_b := Tstb31*Adrag + Tstb32*Acc_Beta + Tstb33*Acc_Alpha in
      (* Roll response to roll command *)
      let Pd_b := (P_b_Cmd - P_b)/Tau_p in
      (* Yaw response to a yaw command *)
      let Y_Acc_Cmd :=  R_b_Cmd*Vmag in
      let Beta_Cmd  :=  (limit Y_Acc_Cmd (- Y_Acc_Max) Y_Acc_Max)/Acc_Per_Beta in
      let Br2       := -Omega0_r*Omega0_r in
      let Ar11      := -Vmag/(Beta_Ref*Rmin_xy) in
      let Ar21      :=  Ar11*(Ar11 - 2.0*Zeta_r*Omega0_r) - Br2 in
      let Ar22      :=  Ar11 - 2.0*Zeta_r*Omega0_r in
      let Rd_b      :=  Ar21*Beta + Ar22*R_b + Br2*Beta_Cmd in
      (* Pitch response to a pitch command *)
      let Z_Acc_Cmd := -Q_b_Cmd*Vmag in
      let Alpha_Cmd :=  (limit Z_Acc_Cmd (- Z_Acc_Max) Z_Acc_Max)/Acc_Per_Alpha in
      let Bq2       :=  Omega0_q*Omega0_q in
      let Aq11      := -Vmag/(Alpha_Ref*Rmin_xz) in
      let Aq21      := -(Aq11*(Aq11 - 2.0*Zeta_q*Omega0_q) + Bq2) in
      let Aq22      :=  Aq11 - 2.0*Zeta_q*Omega0_q in
      let Qd_b      :=  Aq21*Alpha + Aq22*Q_b + Bq2*Alpha_Cmd in
  (*Outputs*)
  union_vars sim [
                 (SvAAEROX_BI_B, AaeroX_bi_b);
                 (SvAAEROY_BI_B, AaeroY_bi_b);
                 (SvAAEROZ_BI_B, AaeroZ_bi_b);
                 (SvALPHA, Alpha);
                 (SvBETA, Beta);
                 (SvPD_B, Pd_b);
                 (SvQD_B, Qd_b);
                 (SvRD_B, Qd_b);
                 (SvRD_B, Rd_b);
                 (SvVMAG, Vmag)
             ].

Definition kinematics (sim: simTy) : simTy :=
  let svars := sim.(vars) in
    let aaerox_bi_b := svGetFloat SvAAEROX_BI_B svars in
    let aaeroy_bi_b := svGetFloat SvAAEROY_BI_B svars in
    let aaeroz_bi_b := svGetFloat SvAAEROZ_BI_B svars in
    let p_b := svGetFloat SvP_B svars in
    let q_b := svGetFloat SvQ_B svars in
    let r_b := svGetFloat SvR_B svars in
    let tib11 := svGetFloat SvTIB11 svars in
    let tib12 := svGetFloat SvTIB12 svars in
    let tib13 := svGetFloat SvTIB13 svars in
    let tib21 := svGetFloat SvTIB21 svars in
    let tib22 := svGetFloat SvTIB22 svars in
    let tib23 := svGetFloat SvTIB23 svars in
    let tib31 := svGetFloat SvTIB31 svars in
    let tib32 := svGetFloat SvTIB32 svars in
    let tib33 := svGetFloat SvTIB33 svars in
    let acc_gravity := svGetFloat SvACC_GRAVITY svars in
    let q0_b := svGetFloat SvQ0_B svars in
    let q1_b := svGetFloat SvQ1_B svars in
    let q2_b := svGetFloat SvQ2_B svars in
    let q3_b := svGetFloat SvQ3_B svars in

    (* Begin math model: *)

    (* Evaluate Euler angles *)
    let stheta_b := limit (-tib13) (-1.0) (1.0) in
    let theta_b := asin(stheta_b) in
    let psi_b := atan2 tib12 tib11 in
    let phi_b := atan2 tib23 tib33 in

    (* Missile velocity rate in ICS WRT ICS *)
    let xdd_bi_i := (tib11 * aaerox_bi_b + tib21 * aaeroy_bi_b + tib31 * aaeroz_bi_b) in
    let ydd_bi_i := (tib12 * aaerox_bi_b + tib22 * aaeroy_bi_b + tib32 * aaeroz_bi_b) in
    let zdd_bi_i := (tib13 * aaerox_bi_b + tib23 * aaeroy_bi_b + tib33 * aaeroz_bi_b) + acc_gravity in

    (* Quaternion constraint equation *)
    let qmag := sqrt(q0_b * q0_b + q1_b * q1_b + q2_b * q2_b + q3_b * q3_b) in
    let q0_b := q0_b / qmag in
    let q1_b := q1_b / qmag in
    let q2_b := q2_b / qmag in
    let q3_b := q3_b / qmag in

    (* Quaternion derivative *)
    let q0d_b := -0.5 * (q1_b * p_b + q2_b * q_b + q3_b * r_b) in
    let q1d_b :=  0.5 * (q0_b * p_b + q2_b * r_b - q3_b * q_b) in
    let q2d_b :=  0.5 * (q0_b * q_b + q3_b * p_b - q1_b * r_b) in
    let q3d_b :=  0.5 * (q0_b * r_b + q1_b * q_b - q2_b * p_b) in

  union_vars sim
             [
               (SvQ0D_B, q0d_b);
               (SvQ1D_B, q1d_b);
               (SvQ2D_B, q2d_b);
               (SvQ3D_B, q3d_b);
               (SvXDD_BI_I, xdd_bi_i);
               (SvYDD_BI_I, ydd_bi_i);
               (SvZDD_BI_I, zdd_bi_i);
               (SvPSI_B, psi_b);
               (SvTHETA_B, theta_b);
               (SvPHI_B, phi_b);
               (SvQ0_B, q0_b);
               (SvQ1_B, q1_b);
               (SvQ2_B, q2_b);
               (SvQ3_B, q3_b)
             ].

Definition target (sim: simTy) : simTy :=
  let svars := sim.(vars) in
  let time := svGetFloat SvT svars in
  let x_ti_i_ic := svGetFloat SvX_TI_I_IC svars in
  let y_ti_i_ic := svGetFloat SvY_TI_I_IC svars in
  let z_ti_i_ic := svGetFloat SvZ_TI_I_IC svars in
  let xd_ti_i := svGetFloat SvXD_TI_I svars in
  let yd_ti_i := svGetFloat SvYD_TI_I svars in
  let zd_ti_i := svGetFloat SvZD_TI_I svars in

  union_vars sim
             [
               (SvX_TI_I, x_ti_i_ic + xd_ti_i * time);
               (SvY_TI_I, y_ti_i_ic + yd_ti_i * time);
               (SvZ_TI_I, z_ti_i_ic + zd_ti_i * time)
             ].

Definition seeker (sim: simTy) : simTy :=
  let svars := sim.(vars) in
  let tib21 := svGetFloat SvTIB21 svars in
  let tib22 := svGetFloat SvTIB22 svars in
  let tib23 := svGetFloat SvTIB23 svars in
  let tib31 := svGetFloat SvTIB31 svars in
  let tib32 := svGetFloat SvTIB32 svars in
  let tib33 := svGetFloat SvTIB33 svars in
  let x_bi_i := svGetFloat SvX_BI_I svars in
  let x_ti_i := svGetFloat SvX_TI_I svars in
  let xd_bi_i := svGetFloat SvXD_BI_I svars in
  let xd_ti_i := svGetFloat SvXD_TI_I svars in
  let y_bi_i := svGetFloat SvY_BI_I svars in
  let y_ti_i := svGetFloat SvY_TI_I svars in
  let yd_bi_i := svGetFloat SvYD_BI_I svars in
  let yd_ti_i := svGetFloat SvYD_TI_I svars in
  let z_bi_i := svGetFloat SvZ_BI_I svars in
  let z_ti_i := svGetFloat SvZ_TI_I svars in
  let zd_bi_i := svGetFloat SvZD_BI_I svars in
  let zd_ti_i := svGetFloat SvZD_TI_I svars in

  let x_tb_i := x_ti_i - x_bi_i in
  let y_tb_i := y_ti_i - y_bi_i in
  let z_tb_i := z_ti_i - z_bi_i in
  let xd_tb_i := xd_ti_i - xd_bi_i in
  let yd_tb_i := yd_ti_i - yd_bi_i in
  let zd_tb_i := zd_ti_i - zd_bi_i in

  (* Missile/target svGetFloat Svange, squared & limited *)
  let range_tb_sq := max small (x_tb_i * x_tb_i + y_tb_i * y_tb_i + z_tb_i * z_tb_i) in

  (* Beam rotation rate WRT inertial space in ICS *)
  let p_s := (y_tb_i * zd_tb_i - z_tb_i * yd_tb_i) / range_tb_sq in
  let q_s := (z_tb_i * xd_tb_i - x_tb_i * zd_tb_i) / range_tb_sq in
  let r_s := (x_tb_i * yd_tb_i - y_tb_i * xd_tb_i) / range_tb_sq in

  (* Beam rotation rate WRT inertial space in BCS *)
  let q_si_b_meas := tib21 * p_s + tib22 * q_s + tib23 * r_s in
  let r_si_b_meas := tib31 * p_s + tib32 * q_s + tib33 * r_s in

  union_vars sim
             [
               (SvP_S, p_s);
               (SvQ_S, q_s);
               (SvQ_SI_B_MEAS, q_si_b_meas);
               (SvR_S, r_s);
               (SvR_SI_B_MEAS, r_si_b_meas);
               (SvRANGE_TB_SQ, range_tb_sq);
               (SvXD_TB_I, xd_tb_i);
               (SvYD_TB_I, yd_tb_i);
               (SvZD_TB_I, zd_tb_i);
               (SvX_TB_I, x_tb_i);
               (SvY_TB_I, y_tb_i);
               (SvZ_TB_I, z_tb_i)
             ].

Definition gyro (sim: simTy) : simTy :=
  let svars := sim.(vars) in
  union_vars sim
             [
               (SvP_G_MEAS, svGetFloat SvP_B svars);
               (SvQ_G_MEAS, svGetFloat SvQ_B svars);
               (SvR_G_MEAS, svGetFloat SvR_B svars)
             ].

Definition flight_computer (sim: simTy) : simTy :=
  let svars := sim.(vars) in
  let p_g_meas := svGetFloat SvP_G_MEAS svars in
  let q_g_meas := svGetFloat SvQ_G_MEAS svars in
  let r_g_meas := svGetFloat SvR_G_MEAS svars in
  let pitch_guidance_gain := svGetFloat SvPITCH_GUIDANCE_GAIN svars in
  let roll_guidance_gain := svGetFloat SvROLL_GUIDANCE_GAIN svars in
  let yaw_guidance_gain := svGetFloat SvYAW_GUIDANCE_GAIN svars in
  let q_b_cmd_bias := svGetFloat SvQ_B_CMD_BIAS svars in
  let del_cmd_per_p_cmd := svGetFloat SvDEL_CMD_PER_P_CMD svars in
  let del_cmd_per_q_cmd := svGetFloat SvDEL_CMD_PER_Q_CMD svars in
  let del_cmd_per_r_cmd := svGetFloat SvDEL_CMD_PER_R_CMD svars in
  let q_si_b_meas := svGetFloat SvQ_SI_B_MEAS svars in
  let r_si_b_meas := svGetFloat SvR_SI_B_MEAS svars in
  let q0_b_est := svGetFloat SvQ0_B_EST svars in
  let q1_b_est := svGetFloat SvQ1_B_EST svars in
  let q2_b_est := svGetFloat SvQ2_B_EST svars in
  let q3_b_est := svGetFloat SvQ3_B_EST svars in

  (* Begin flight computer emulation: *)

  (* Navigation section *)
  (* Evaluate the ICS to BCS transformation matrix *)
  let tib11_est := q0_b_est * q0_b_est + q1_b_est * q1_b_est - q2_b_est * q2_b_est - q3_b_est * q3_b_est in
  let tib12_est := 2.0 * (q1_b_est * q2_b_est + q0_b_est * q3_b_est) in
  let tib13_est := 2.0 * (q1_b_est * q3_b_est - q0_b_est * q2_b_est) in
  let tib23_est := 2.0 * (q2_b_est * q3_b_est + q0_b_est * q1_b_est) in
  let tib33_est := q0_b_est * q0_b_est + q3_b_est * q3_b_est - q1_b_est * q1_b_est - q2_b_est * q2_b_est in

  (* Evaluate Euler roll angle *)
  let stheta_b_est := limit (-tib13_est) (-1.0) (1.0) in
  let psi_b_est := atan2 tib12_est tib11_est in
  let theta_b_est := asin stheta_b_est in
  let phi_b_est := atan2 tib23_est tib33_est in

  (* Quaternion constraint equation *)
  let qmag_est := sqrt (q0_b_est * q0_b_est + q1_b_est * q1_b_est + q2_b_est * q2_b_est + q3_b_est * q3_b_est) in
  let q0_b_est := q0_b_est / qmag_est in
  let q1_b_est := q1_b_est / qmag_est in
  let q2_b_est := q2_b_est / qmag_est in
  let q3_b_est := q3_b_est / qmag_est in

  (* Quaternion derivative *)
  let q0d_b_est := -0.5 * (q1_b_est * p_g_meas + q2_b_est * q_g_meas + q3_b_est * r_g_meas) in
  let q1d_b_est := 0.5 * (q0_b_est * p_g_meas + q2_b_est * r_g_meas - q3_b_est * q_g_meas) in
  let q2d_b_est := 0.5 * (q0_b_est * q_g_meas + q3_b_est * p_g_meas - q1_b_est * r_g_meas) in
  let q3d_b_est := 0.5 * (q0_b_est * r_g_meas + q1_b_est * q_g_meas - q2_b_est * p_g_meas) in

  (* Maintain zero roll angle *)
  let p_b_cmd := roll_guidance_gain * phi_b_est in

  (* Compute yaw & pitch rate command via proportional navigation *)
  let q_b_cmd := pitch_guidance_gain * q_si_b_meas + q_b_cmd_bias * cos(theta_b_est) in
  let r_b_cmd := yaw_guidance_gain * r_si_b_meas in

  (* Autopilot *)
  let del_p_cmd := del_cmd_per_p_cmd * p_b_cmd in
  let del_q_cmd := del_cmd_per_q_cmd * q_b_cmd in
  let del_r_cmd := del_cmd_per_r_cmd * r_b_cmd in

  (* Fin mixing *)
  let fin_1_cmd := -del_q_cmd - del_p_cmd in
  let fin_2_cmd := -del_r_cmd - del_p_cmd in
  let fin_3_cmd := del_q_cmd - del_p_cmd in
  let fin_4_cmd := del_r_cmd - del_p_cmd in

 union_vars sim
             [
               (SvPSI_B_EST, psi_b_est);
               (SvTHETA_B_EST, theta_b_est);
               (SvPHI_B_EST, phi_b_est);
               (SvQ0D_B_EST, q0d_b_est);
               (SvQ1D_B_EST, q1d_b_est);
               (SvQ2D_B_EST, q2d_b_est);
               (SvQ3D_B_EST, q3d_b_est);
               (SvP_B_CMD, p_b_cmd);
               (SvQ_B_CMD, q_b_cmd);
               (SvR_B_CMD, r_b_cmd);
               (SvQ0_B_EST, q0_b_est);
               (SvQ1_B_EST, q1_b_est);
               (SvQ2_B_EST, q2_b_est);
               (SvQ3_B_EST, q3_b_est);
               (SvFIN_1_CMD, fin_1_cmd);
               (SvFIN_2_CMD, fin_2_cmd);
               (SvFIN_3_CMD, fin_3_cmd);
               (SvFIN_4_CMD, fin_4_cmd)
             ].

Definition differential_equations (sim0: simTy) : simTy :=
  let sim1 := kinematics sim0 in
  let sim2 := target sim1 in
  let sim3 := seeker sim2 in
  let sim4 := gyro sim3 in
  let sim5 := flight_computer sim4 in
  let sim6 := airframe_response sim5 in
  sim6.
