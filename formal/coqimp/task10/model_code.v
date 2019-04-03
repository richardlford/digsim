Require Export Task.driver_state.
Require Export Task.trig.
Import ListNotations.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import RecordSetNotations'.
Open Scope D_scope.
Import CoqTrig.

Definition z_terminate_sim_event := {| key := "z_terminate_sim_event"; time := 0%D |}.

Definition sq (f: float) := f * f.


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
  sim[[log_entries ::= (fun oldlog => miss_entry :: oldlog)]].

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
        if (time_to_go <? 0%D) then
          let sim1 := log_miss sim time_to_go in
          let flags2 := sim1.(flags)[[end_of_run := true]] in
          sim1[[flags := flags2]]
        else
          sim
    in
    (result_sim, None).

Definition model_handlers :=
  ("z_terminate_sim_event", z_terminate_sim_event_func) :: 
  driver_default_handlers.

(*+ Initial the modules *)
Definition actuator_init (sim: simTy) := sim.
Definition aero_init (sim: simTy) := sim.
Definition mass_init (sim: simTy) := sim.

Definition kinematics_init (sim: simTy) :=
  let svars := sim.(vars) in
  let psi_b := svGetFloat SvPSI_B_IC_DG svars / rd_to_dg in
  let theta_b := svGetFloat SvTHETA_B_IC_DG svars / rd_to_dg in
  let phi_b := svGetFloat SvPHI_B_IC_DG svars / rd_to_dg in
  let p_b := svGetFloat SvP_B_IC_DG svars / rd_to_dg in
  let q_b := svGetFloat SvQ_B_IC_DG svars / rd_to_dg in
  let r_b := svGetFloat SvR_B_IC_DG svars / rd_to_dg in

  let cpsio2 := cos("0.5"#D * psi_b) in
  let cthetao2 := cos("0.5"#D * theta_b) in
  let cphio2 := cos("0.5"#D * phi_b) in
  let spsio2 := sin("0.5"#D * psi_b) in
  let sthetao2 := sin("0.5"#D * theta_b) in
  let sphio2 := sin("0.5"#D * phi_b) in
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
  let cpsio2 := cos("0.5"#D * psi_b_est_dg / rd_to_dg) in
  let cthetao2 := cos("0.5"#D * theta_b_est_dg / rd_to_dg) in
  let cphio2 := cos("0.5"#D * phi_b_est_dg / rd_to_dg) in
  let spsio2 := sin("0.5"#D * psi_b_est_dg / rd_to_dg) in
  let sthetao2 := sin("0.5"#D * theta_b_est_dg / rd_to_dg) in
  let sphio2 := sin("0.5"#D * phi_b_est_dg / rd_to_dg) in

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
  let sim1 := actuator_init sim0 in
  let sim2 := aero_init sim1 in
  let sim3 := mass_init sim2 in
  let sim4 := kinematics_init sim3 in
  let sim5 := gyro_init sim4 in
  let sim6 := target_init sim5 in
  let sim7 := seeker_init sim6 in
  let sim8 := flight_computer_init sim7 in
  let sim9 := union_vars sim8
                         [
                           (SvROLL_GUIDANCE_GAIN, "-20.0"#D);
                           (SvPITCH_GUIDANCE_GAIN, "4.0"#D);
                           (SvX_TI_I, "500.0"#D);
                           (SvY_TI_I, "-250.0"#D)
                         ] in
  sim9[[sim_events ::= (fun others => z_terminate_sim_event::others)]].


Definition limit (x l u  : float) : float :=
  if (x <? l) then l
  else if (x >? u) then u
  else  x.

Definition max (x y: float) : float :=
  if x >? y then x else y.

Definition actuator (sim: simTy) : simTy :=
  let svars := sim.(vars) in 
  let fl := svGetFloat SvFIN_LIMIT svars in
  let fin_1_cmd := svGetFloat SvFIN_1_CMD svars in
  let fin_2_cmd := svGetFloat SvFIN_2_CMD svars in
  let fin_3_cmd := svGetFloat SvFIN_3_CMD svars in
  let fin_4_cmd := svGetFloat SvFIN_4_CMD svars in
  union_vars sim
             [
               (SvFIN_1_POSITION, limit fin_1_cmd  (-fl)  fl) ;
               (SvFIN_2_POSITION, limit fin_2_cmd  (-fl)  fl) ;
               (SvFIN_3_POSITION, limit fin_3_cmd  (-fl)  fl) ;
               (SvFIN_4_POSITION, limit fin_4_cmd  (-fl)  fl) 
             ].

Definition aero (sim: simTy) : simTy :=
  let svars := sim.(vars) in
  let air_density := svGetFloat SvAIR_DENSITY svars in
  let ref_area := svGetFloat SvREF_AREA svars in
  let ref_length := svGetFloat SvREF_LENGTH svars in
  let cx_base := svGetFloat SvCX_BASE svars in
  let cx_per_alpha_total := svGetFloat SvCX_PER_ALPHA_TOTAL svars in
  let cy_per_beta := svGetFloat SvCY_PER_BETA svars in
  let cz_per_alpha := svGetFloat SvCZ_PER_ALPHA svars in
  let cmy_per_alpha := svGetFloat SvCMY_PER_ALPHA svars in
  let cmz_per_beta := svGetFloat SvCMZ_PER_BETA svars in
  let cmx_per_delp := svGetFloat SvCMX_PER_DELP svars in
  let cmy_per_delq := svGetFloat SvCMY_PER_DELQ svars in
  let cmz_per_delr := svGetFloat SvCMZ_PER_DELR svars in
  let cmp := svGetFloat SvCMP svars in
  let cmq := svGetFloat SvCMQ svars in
  let cmr := svGetFloat SvCMR svars in
  let xd_bi_i := svGetFloat SvXD_BI_I svars in
  let yd_bi_i := svGetFloat SvYD_BI_I svars in
  let zd_bi_i := svGetFloat SvZD_BI_I svars in
  let p_b := svGetFloat SvP_B svars in
  let q_b := svGetFloat SvQ_B svars in
  let r_b := svGetFloat SvR_B svars in
  let q0_b := svGetFloat SvQ0_B svars in
  let q1_b := svGetFloat SvQ1_B svars in
  let q2_b := svGetFloat SvQ2_B svars in
  let q3_b := svGetFloat SvQ3_B svars in
  let fin_1_position := svGetFloat SvFIN_1_POSITION svars in
  let fin_2_position := svGetFloat SvFIN_2_POSITION svars in
  let fin_3_position := svGetFloat SvFIN_3_POSITION svars in
  let fin_4_position := svGetFloat SvFIN_4_POSITION svars in

  (* Begin math model: *)

  (* Evaluate ICS to BCS transformation matrix *)
  let tib11 := q0_b * q0_b + q1_b * q1_b - q2_b * q2_b - q3_b * q3_b in
  let tib12 := "2.0"#D * (q1_b * q2_b + q0_b * q3_b) in
  let tib13 := "2.0"#D * (q1_b * q3_b - q0_b * q2_b) in
  let tib21 := "2.0"#D * (q1_b * q2_b - q0_b * q3_b) in
  let tib22 := q0_b * q0_b + q2_b * q2_b - q1_b * q1_b - q3_b * q3_b in
  let tib23 := "2.0"#D * (q2_b * q3_b + q0_b * q1_b) in
  let tib31 := "2.0"#D * (q1_b * q3_b + q0_b * q2_b) in
  let tib32 := "2.0"#D * (q2_b * q3_b - q0_b * q1_b) in
  let tib33 := q0_b * q0_b + q3_b * q3_b - q1_b * q1_b - q2_b * q2_b in

  (* Missile velocity WRT ICS origin in BCS *)
  let xd_bi_b := xd_bi_i * tib11 + yd_bi_i * tib12 + zd_bi_i * tib13 in
  let yd_bi_b := xd_bi_i * tib21 + yd_bi_i * tib22 + zd_bi_i * tib23 in
  let zd_bi_b := xd_bi_i * tib31 + yd_bi_i * tib32 + zd_bi_i * tib33 in
  let velsq := xd_bi_i * xd_bi_i + yd_bi_i * yd_bi_i + zd_bi_i * zd_bi_i in
  let vmag := sqrt(velsq) in

  (* Angle of attack, sideslip angle, and total angle-of-attack *)
  let alpha := atan2 zd_bi_b xd_bi_b in
  let beta := atan2 yd_bi_b xd_bi_b in
  let alpha_total := atan2 (sqrt (yd_bi_b * yd_bi_b + zd_bi_b * zd_bi_b)) xd_bi_b in

  (* Dynamic pressure and multipliers for forces and moments *)
  let qbar_b := "0.5"#D * air_density * velsq in
  let qbarsref := qbar_b * ref_area in
  let qbarsreflref := qbarsref * ref_length in
  let lrefo2vmag := ref_length / ("2.0"#D * vmag) in

  (* Equivalent fin deflections *)
  let del_eff_p := "-0.25"#D * (fin_1_position + fin_2_position + fin_3_position + fin_4_position) in
  let del_eff_q := "0.5"#D * (fin_3_position - fin_1_position) in
  let del_eff_r := "0.5"#D * (fin_4_position - fin_2_position) in

  (* Aerodynamics forces *)
  let faerox_bi_b := (cx_base + cx_per_alpha_total * alpha_total) * qbarsref in
  let faeroy_bi_b := cy_per_beta * beta * qbarsref in
  let faeroz_bi_b := cz_per_alpha * alpha * qbarsref in

  (* Aerodynamics moments *)
  let maerox_bi_b := qbarsreflref * (cmx_per_delp * del_eff_p + cmp * p_b * lrefo2vmag) in
  let maeroy_bi_b := qbarsreflref * (cmy_per_alpha * alpha + cmy_per_delq * del_eff_q + cmq * q_b * lrefo2vmag) in
  let maeroz_bi_b := qbarsreflref * (cmz_per_beta * beta + cmz_per_delr * del_eff_r + cmr * r_b * lrefo2vmag) in
  union_vars sim
             [
               (SvTIB11, tib11);
               (SvTIB12, tib12);
               (SvTIB13, tib13);
               (SvTIB21, tib21);
               (SvTIB22, tib22);
               (SvTIB23, tib23);
               (SvTIB31, tib31);
               (SvTIB32, tib32);
               (SvTIB33, tib33);
               (SvALPHA, alpha);
               (SvBETA, beta);
               (SvALPHA_TOTAL, alpha_total);
               (SvFAEROX_BI_B, faerox_bi_b);
               (SvFAEROY_BI_B, faeroy_bi_b);
               (SvFAEROZ_BI_B, faeroz_bi_b);
               (SvMAEROX_BI_B, maerox_bi_b);
               (SvMAEROY_BI_B, maeroy_bi_b);
               (SvMAEROZ_BI_B, maeroz_bi_b)
             ].

Definition mass (sim: simTy) : simTy := sim.

Definition kinematics (sim: simTy) : simTy :=
  let svars := sim.(vars) in
    let faerox_bi_b := svGetFloat SvFAEROX_BI_B svars in
    let faeroy_bi_b := svGetFloat SvFAEROY_BI_B svars in
    let faeroz_bi_b := svGetFloat SvFAEROZ_BI_B svars in
    let maerox_bi_b := svGetFloat SvMAEROX_BI_B svars in
    let maeroy_bi_b := svGetFloat SvMAEROY_BI_B svars in
    let maeroz_bi_b := svGetFloat SvMAEROZ_BI_B svars in
    let ixx_b := svGetFloat SvIXX_B svars in
    let iyy_b := svGetFloat SvIYY_B svars in
    let izz_b := svGetFloat SvIZZ_B svars in
    let mass_b := svGetFloat SvMASS_B svars in
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
    let stheta_b := limit (-tib13) ("-1.0"#D) ("1.0"#D) in
    let theta_b := asin(stheta_b) in
    let psi_b := atan2 tib12 tib11 in
    let phi_b := atan2 tib23 tib33 in

    (* Missile velocity rate in ICS WRT ICS *)
    let xdd_bi_i := (tib11 * faerox_bi_b + tib21 * faeroy_bi_b + tib31 * faeroz_bi_b) / mass_b in
    let ydd_bi_i := (tib12 * faerox_bi_b + tib22 * faeroy_bi_b + tib32 * faeroz_bi_b) / mass_b in
    let zdd_bi_i := (tib13 * faerox_bi_b + tib23 * faeroy_bi_b + tib33 * faeroz_bi_b) / mass_b + acc_gravity in

    (* Angular rate derivatives *)
    let pd_b := maerox_bi_b / ixx_b in
    let qd_b := (maeroy_bi_b + p_b * r_b * (izz_b - ixx_b)) / iyy_b in
    let rd_b := (maeroz_bi_b + q_b * p_b * (ixx_b - iyy_b)) / izz_b in

    (* Quaternion constraint equation *)
    let qmag := sqrt(q0_b * q0_b + q1_b * q1_b + q2_b * q2_b + q3_b * q3_b) in
    let q0_b := q0_b / qmag in
    let q1_b := q1_b / qmag in
    let q2_b := q2_b / qmag in
    let q3_b := q3_b / qmag in

    (* Quaternion derivative *)
    let q0d_b := "-0.5"#D * (q1_b * p_b + q2_b * q_b + q3_b * r_b) in
    let q1d_b :=  "0.5"#D * (q0_b * p_b + q2_b * r_b - q3_b * q_b) in
    let q2d_b :=  "0.5"#D * (q0_b * q_b + q3_b * p_b - q1_b * r_b) in
    let q3d_b :=  "0.5"#D * (q0_b * r_b + q1_b * q_b - q2_b * p_b) in

  union_vars sim
             [
               (SvQ0D_B, q0d_b);
               (SvQ1D_B, q1d_b);
               (SvQ2D_B, q2d_b);
               (SvQ3D_B, q3d_b);
               (SvPD_B, pd_b);
               (SvQD_B, qd_b);
               (SvRD_B, rd_b);
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
  let tib12_est := "2.0"#D * (q1_b_est * q2_b_est + q0_b_est * q3_b_est) in
  let tib13_est := "2.0"#D * (q1_b_est * q3_b_est - q0_b_est * q2_b_est) in
  let tib23_est := "2.0"#D * (q2_b_est * q3_b_est + q0_b_est * q1_b_est) in
  let tib33_est := q0_b_est * q0_b_est + q3_b_est * q3_b_est - q1_b_est * q1_b_est - q2_b_est * q2_b_est in

  (* Evaluate Euler roll angle *)
  let stheta_b_est := limit (-tib13_est) ("-1.0"#D) ("1.0"#D) in
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
  let q0d_b_est := "-0.5"#D * (q1_b_est * p_g_meas + q2_b_est * q_g_meas + q3_b_est * r_g_meas) in
  let q1d_b_est := "0.5"#D * (q0_b_est * p_g_meas + q2_b_est * r_g_meas - q3_b_est * q_g_meas) in
  let q2d_b_est := "0.5"#D * (q0_b_est * q_g_meas + q3_b_est * p_g_meas - q1_b_est * r_g_meas) in
  let q3d_b_est := "0.5"#D * (q0_b_est * r_g_meas + q1_b_est * q_g_meas - q2_b_est * p_g_meas) in

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
  let sim1 := actuator sim0 in
  let sim2 := aero sim1 in
  let sim3 := mass sim2 in
  let sim4 := kinematics sim3 in
  let sim5 := target sim4 in
  let sim6 := seeker sim5 in
  let sim7 := gyro sim6 in
  let sim8 := flight_computer sim7 in
  sim8.
