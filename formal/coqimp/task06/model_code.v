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
  let x := svGetFloat SvX vars in
  let xd := svGetFloat SvXD vars in
  let z := svGetFloat SvZ vars in
  let zd := svGetFloat SvZD vars in
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
    let z := svGetFloat SvZ vars in
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

Definition init_sim (sim: simTy) :=
  let sim1 := log_sim "init_sim:sim" sim in
  let sim2 := sim1[[sim_events ::= (fun others => z_terminate_sim_event::others)]] in
  sim2.

Definition differential_equations (sim: simTy) : simTy :=
  let vars := sim.(vars) in
  let x := svGetFloat SvX vars in
  let z := svGetFloat SvZ vars in
  let theta := svGetFloat SvTHETA vars in
  let velocity := svGetFloat SvVELOCITY vars in
  let guidance_gain := svGetFloat SvGUIDANCE_GAIN vars in

  (* Begin math model *)
  (* Calculate velocities *)
  let xd := velocity * CoqTrig.cos(theta) in
  let zd := -velocity * CoqTrig.sin(theta) in
  (* Calculate true LOS rate *)
  let q_s := (z * xd - x * zd) / (x * x + z * z) in
  (* Calculate measured LOS rate *)
  let q_s_meas := q_s in
  (* Calculate guidance comand *)
  let theta_dot_cmd := guidance_gain * q_s_meas in
  (* Calculate airframe rate *)
  let theta_dot := theta_dot_cmd in
  union_vars
    sim
    [
      (SvXD,            xd);
        (SvZD,            zd);
        (SvTHETA_DOT,     theta_dot);
        (SvTHETA_DOT_CMD, theta_dot_cmd);
        (SvQ_S,           q_s);
        (SvQ_S_MEAS,      q_s_meas)
    ].
