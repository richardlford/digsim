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

Definition log_miss (sim: simTy) : simTy :=
  let vars := sim.(vars) in 
  let t := svGetFloat SvT vars in
  let x := svGetFloat SvX_BI_I vars in
  let xd := svGetFloat SvXD_BI_I vars in
  let z := svGetFloat SvZ_BI_I vars in
  let zd := svGetFloat SvZD_BI_I vars in
  let dt_miss := (x * xd + z * zd) / (sq xd + sq zd) in
  let r_miss := sqrt(sq (x - xd * dt_miss) + sq (z - zd * dt_miss)) in
  let miss_vars :=
      [("t", print_float t);
       ("r_miss", print_float r_miss);
       ("dt_miss", print_float dt_miss)] in
  let miss_entry := {| le_caption := "log_miss"; le_vars := miss_vars; le_events := [] |} in
  let result_sim := sim[[log_entries ::= (fun oldlog => miss_entry :: oldlog)]] in
  result_sim.

Definition z_terminate_sim_event_func  : event_function_signature :=
  fun (this: eventTy) (sim: simTy) =>
    let vars := sim.(vars) in
    let z := svGetFloat SvZ_BI_I vars in
    let result_sim :=
        if (z >=? 0%D) then
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
Definition seeker_init (sim: simTy) := sim.
Definition flight_computer_init (sim: simTy) := sim.
Definition airframe_response_init (sim: simTy) := sim.


Definition init_sim (sim: simTy) :=
  let sim1 := kinematics_init sim in
  let sim2 := seeker_init sim1 in
  let sim3 := flight_computer_init sim2 in
  let sim4 := airframe_response_init sim3 in
  let sim5 := sim4[[sim_events ::= (fun others => z_terminate_sim_event::others)]] in
  sim5.

Definition kinematics (sim: simTy) : simTy :=
  let vars := sim.(vars) in 
  let velocity := svGetFloat SvVELOCITY vars in
  let theta := svGetFloat SvTHETA_B vars in
  let xd := velocity * CoqTrig.cos(theta) in
  let zd := -velocity * CoqTrig.sin(theta) in
  union_vars sim [ (SvXD_BI_I, xd); (SvZD_BI_I, zd) ].

Definition seeker (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  let x := svGetFloat SvX_BI_I vars in
  let xd := svGetFloat SvXD_BI_I vars in
  let z := svGetFloat SvZ_BI_I vars in
  let zd := svGetFloat SvZD_BI_I vars in
  (* Calculate true LOS rate *)
  let q_s := (z * xd - x * zd) / (x * x + z * z) in
  (* Calculate measured LOS rate *)
  let q_s_meas := q_s in
  union_vars sim [ (SvQ_S, q_s); (SvQ_S_MEAS, q_s_meas) ].

Definition flight_computer (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  let guidance_gain := svGetFloat SvGUIDANCE_GAIN vars in
  let q_s_meas := svGetFloat SvQ_S_MEAS vars in
  (* Calculate guidance comand *)
  let theta_dot_cmd := guidance_gain * q_s_meas in
  set_var SvTHETA_DOT_B_CMD theta_dot_cmd sim.

  
Definition airframe_response (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  set_var SvTHETA_DOT_B (svGetFloat SvTHETA_DOT_B_CMD vars) sim.
    
Definition differential_equations (sim: simTy) : simTy :=
  let sim1 := kinematics sim in
  let sim2 := seeker sim1 in
  let sim3 := flight_computer sim2 in
  let sim4 := airframe_response sim3 in
  sim4.
