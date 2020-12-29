Require Export Task.driver_state.
Require Export Task.trig.
Import ListNotations.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import RecordSetNotations'.
Open Scope D_scope.

Definition z_terminate_sim_event := {| key := "z_terminate_sim_event"; time := 0%D |}.

Definition sq (f: float) := f * f.
Arguments sq / f.

Definition log_miss (sim: simTy) : simTy :=
  let vars := sim.(vars) in 
  let t := svGetFloat SvT vars in
  let x := svGetFloat SvX_BI_I vars in
  let xd := svGetFloat SvXD_BI_I vars in
  let z := svGetFloat SvZ_BI_I vars in
  let zd := svGetFloat SvZD_BI_I vars in
  let xt := svGetFloat SvX_TI_I vars in
  let xtd := svGetFloat SvXD_TI_I vars in
  let zt := svGetFloat SvZ_TI_I vars in
  let ztd := svGetFloat SvZD_TI_I vars in
  let xb := x - xt in
  let xbd := xd - xtd in
  let zb := z - zt in
  let zbd := zd - ztd in
  let dt_miss := (xb * xbd + zb * zbd) / (sq xbd + sq zbd) in
  let r_miss := sqrt(sq (xb - xbd * dt_miss) + sq (zb - zbd * dt_miss)) in
  let miss_vars :=
      [("t", print_float t);
       ("r_miss", print_float r_miss);
       ("dt_miss", print_float dt_miss)] in
  let miss_entry := {| le_caption := "log_miss"; le_vars := miss_vars; le_events := [] |} in
  let result_sim := sim[[log_entries ::= (fun oldlog => miss_entry :: oldlog)]] in
  result_sim.
Arguments log_miss !sim.

Definition z_terminate_sim_event_func  : event_function_signature :=
  fun (this: eventTy) (sim: simTy) =>
    let vars := sim.(vars) in
    let x := svGetFloat SvX_BI_I vars in
    let xd := svGetFloat SvXD_BI_I vars in
    let z := svGetFloat SvZ_BI_I vars in
    let zd := svGetFloat SvZD_BI_I vars in
    let xt := svGetFloat SvX_TI_I vars in
    let xtd := svGetFloat SvXD_TI_I vars in
    let zt := svGetFloat SvZ_TI_I vars in
    let ztd := svGetFloat SvZD_TI_I vars in
    let xb := x - xt in
    let xbd := xd - xtd in
    let zb := z - zt in
    let zbd := zd - ztd in
    let time_to_go := - ((xb * xbd + zb * zbd)) / ((sq xbd) + (sq zbd)) in
    let result_sim :=
        if (time_to_go <? 0%D) then
          let sim1 := log_miss sim in
          let flags2 := sim1.(flags)[[end_of_run := true]] in
          let sim2 := sim1[[flags := flags2]] in
          sim2
        else
          sim
    in
    (result_sim, None).

Definition model_handlers :=
  ("z_terminate_sim_event", z_terminate_sim_event_func) :: 
  driver_default_handlers.

(* The init functions are all nops *)
Definition kinematics_init (sim: simTy) := sim.
Arguments kinematics_init !sim.

Definition seeker_init (sim: simTy) := sim.
Arguments seeker_init !sim.

Definition flight_computer_init (sim: simTy) := sim.
Arguments flight_computer_init !sim.

Definition airframe_response_init (sim: simTy) := sim.
Arguments airframe_response_init !sim.

Definition target_init (sim: simTy) := sim.
Arguments target_init !sim.

Definition init_sim (sim: simTy) :=
  let sim1 := kinematics_init sim in
  let sim2 := target_init sim1 in
  let sim3 := seeker_init sim2 in
  let sim4 := flight_computer_init sim3 in
  let sim5 := airframe_response_init sim4 in
  let sim6 := sim5[[sim_events ::= (fun others => z_terminate_sim_event::others)]] in
  sim6.
Arguments init_sim / sim.

Definition kinematics (sim: simTy) : simTy :=
  let vars := sim.(vars) in 
  let v_x := svGetFloat SvAAEROX_BI_B vars in
  let v_z := svGetFloat SvAAEROZ_BI_B vars in
  let theta := svGetFloat SvTHETA_B vars in
  let g := svGetFloat SvGRAVITY vars in
  let xdd := (v_x * CoqTrig.cos(theta) + v_z * CoqTrig.sin(theta)) in
  let zdd := (-v_x * CoqTrig.sin(theta) + v_z * CoqTrig.cos(theta) + g) in
  union_vars sim [ (SvXDD_BI_I, xdd); (SvZDD_BI_I, zdd) ].
Arguments kinematics !sim.

Definition target (sim: simTy) : simTy := sim.
(*  let vars := sim.(vars) in
  let t := svGetFloat SvT vars in
  let x := svGetFloat SvX_TI_I vars in
  let xd := svGetFloat SvXD_TI_I vars in
  let z := svGetFloat SvZ_TI_I vars in
  let zd := svGetFloat SvZD_TI_I vars in
  let new_x := x + xd * t in
  let new_z := z + zd * t in (*Isn't this what modelPairs is for?*)
  union_vars sim [ (SvX_TI_I, new_x); (SvZ_TI_I, new_z) ].*)
Arguments target !sim.

Definition seeker (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  let x := (svGetFloat SvX_BI_I vars) - (svGetFloat SvX_TI_I vars) in
  let xd := (svGetFloat SvXD_BI_I vars) - (svGetFloat SvXD_TI_I vars) in
  let z := (svGetFloat SvZ_BI_I vars) - (svGetFloat SvZ_TI_I vars) in
  let zd := (svGetFloat SvZD_BI_I vars) - (svGetFloat SvZD_TI_I vars) in
  (* Calculate true LOS rate *)
  let q_s := (z * xd - x * zd) / ((sq x) + (sq z)) in
  (* Calculate measured LOS rate *)
  let q_s_meas := q_s in
  union_vars sim [ (SvQ_S, q_s); (SvQ_S_MEAS, q_s_meas) ].
Arguments seeker !sim.

Definition flight_computer (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  let guidance_gain := svGetFloat SvGUIDANCE_GAIN vars in
  let q_s_meas := svGetFloat SvQ_S_MEAS vars in
  (* Calculate guidance comand *)
  let q_b_cmd := guidance_gain * q_s_meas in
  set_var SvQ_B_CMD q_b_cmd sim.
  Arguments flight_computer !sim.

  Definition max (x y:float) : float :=
    match x <? y with
    |true => y
    |false => x
    end.

  Definition min (x y:float) : float :=
    match x <? y with
    |true => x
    |false => y
    end.

  Definition limit (v x y:float) : float :=
    match v <? (min x y) with
    |true => (min x y)
    |false =>  match (max x y) <? v with
              |true => (max x y)
              |false => v
              end
    end.
 
Definition airframe_response (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  (*Inputs*)
  let alpha_ref := (svGetFloat SvALPHA_REF vars) in
  let drag_per_velsq := (svGetFloat SvDRAG_PER_VELSQ vars) in
  let omega0_q := (svGetFloat SvOMEGA0_q vars) in
  let q_b := (svGetFloat SvQ_B vars) in
  let q_b_cmd := (svGetFloat SvQ_B_CMD vars) in
  let rmin_xz := (svGetFloat SvRMIN_XZ vars) in
  let xd := (svGetFloat SvXD_BI_I vars) in
  let zd := (svGetFloat SvZD_BI_I vars) in
  let zeta_q := (svGetFloat SvZETA_Q vars) in
  let theta := (svGetFloat SvTHETA_B vars) in
  (*Missile Velocity in missile axis*)
  let xdb := (xd * CoqTrig.cos(theta) - zd * CoqTrig.sin(theta)) in
  let zdb := (xd * CoqTrig.sin(theta) + zd * CoqTrig.cos(theta)) in
  (*Missile Velocity WRT ICS origin in BCS*)
  let velsq := ((sq xd) + (sq zd)) in
  let vmag := (sqrt velsq) in
  (*Angle of attack*)
  let alpha := (CoqTrig.atan2 zdb xdb) in
  (*Translational accelerations due to aero in stability axis*)
  let adrag := (velsq * drag_per_velsq) in
  let z_acc_max := (velsq / rmin_xz) in
  let acc_per_alpha := - (max ("0.000001"#D) (z_acc_max / alpha_ref)) in
  let acc_alpha := acc_per_alpha*alpha in
  (*Translational accelerations due to aero in BCS*)
  let aaerox_bi_b := (adrag*CoqTrig.cos(alpha) - acc_alpha*CoqTrig.sin(alpha)) in
  let aaeroz_bi_b := (adrag*CoqTrig.sin(alpha) + acc_alpha*CoqTrig.cos(alpha)) in
  (*Pitch response to a pitch command*)
  let alpha_cmd := (limit (-q_b_cmd*vmag) (-z_acc_max) (z_acc_max))/acc_per_alpha in
  let bq2 := sq omega0_q in
  let aq11 := -vmag/(alpha_ref*rmin_xz) in
  let aq21 := -(aq11*(aq11 - ("2.0"#D) * zeta_q * omega0_q) + bq2) in
  let aq22 := aq11 - ("2.0"#D) * zeta_q * omega0_q in
  let qd_b := aq21 * alpha + aq22 * q_b + bq2 * alpha_cmd in
  union_vars sim [
               (SvAAEROX_BI_B, aaerox_bi_b);
               (SvAAEROZ_BI_B, aaeroz_bi_b);
               (SvZ_ACC_MAX, z_acc_max);
               (SvACC_ALPHA, acc_alpha);
               (SvACC_PER_ALPHA, acc_per_alpha);
               (SvQD_B, qd_b);
               (SvVELSQ, velsq);
               (SvVMAG, vmag);
               (SvTHETA_DOT_B, q_b)
             ].
Arguments airframe_response !sim.

Definition differential_equations (sim: simTy) : simTy :=
  let sim1 := kinematics sim in
  let sim2 := target sim1 in
  let sim3 := seeker sim2 in
  let sim4 := flight_computer sim3 in
  let sim5 := airframe_response sim4 in
  sim5.
Arguments differential_equations !sim.
