Require Export Task.driver_state.

Import ListNotations.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import RecordSetNotations'.
Open Scope D_scope.

(* Constants needed by the model *)
Definition fhalf := (* Eval compute in *) strToFloat' "0.5".
Definition fone := (* Eval compute in *) strToFloat' "1.0".
Definition ftwo := (* Eval compute in *) strToFloat' "2.0".
Definition f99 := (* Eval compute in *) strToFloat' "99.0".


Definition bounceEvent :=
  {| key := "flip_xd_at_bounce_event";
     time := f99
  |}.

Definition flip_xd_at_bounce_event_func (this: eventTy) (sim: simTy) : (simTy * option float) :=
  let sim' := log_sim "flip_xd_at_bounce_event_func: sim" sim in
  let vars := sim'.(vars) in
  let coeff_of_rest := svGetFloat SvCOEFF_OF_REST vars in
  let xd := svGetFloat SvXD vars in
  let new_xd := - (coeff_of_rest * xd) in
  let vars' := SvTree.set SvXD new_xd vars in
  let new_flags := sim'.(flags)[[evaluate_xd := true]] in
  let sim2 := set_vars sim' vars' in
  let result_sim := sim2[[flags := new_flags]] in
  let result_log := log_sim "flip_xd_at_bounce_event_func" result_sim in
  (result_log, Some f99).

Definition model_handlers :=
  ("flip_xd_at_bounce_event", flip_xd_at_bounce_event_func) :: driver_default_handlers.

Definition init_sim (sim: simTy) :=
  let sim' := log_sim "init_sim:sim" sim in
  let result_sim := sim[[sim_events ::= (fun evs => bounceEvent :: evs)]] in
  let result_sim' := log_sim "init_sim: result" result_sim in
  result_sim'.

Definition differential_equations (sim: simTy) : simTy :=
  let sim_log := log_sim "differential_equations" sim in
  let vars := sim_log.(vars) in
  let t := svGetFloat SvT vars in
  let x := svGetFloat SvX vars in
  let xd := svGetFloat SvXD vars in
  let dt_min := svGetFloat SvDT_MIN vars in
  let dt_max := svGetFloat SvDT_MAX vars in
  let gravity := svGetFloat SvGRAVITY vars in
  let t_stop := svGetFloat SvT_STOP vars in

  let xdd := -gravity in
  let sim1 := set_var SvXDD xdd sim_log in
  let sim1log := log_sim "differential_equations:sim1" sim1 in
  let est_max := x + xd * dt_max + fhalf * xdd * dt_max * dt_max in
  let dt_impact := t_stop + fone in
  let dt_impact2 :=
      if (est_max <=? 0%D) then
        let est_min := x + xd * dt_min + fhalf * xdd * dt_min * dt_min in
        if (est_min <=? 0%D) then
            0%D
        else
            let dt_impact3 := (-xd - sqrt(xd * xd - ftwo * x * xdd)) / (ftwo * x) in
            if (dt_min - dt_impact3 >? epsilon) then
              (-xd + sqrt(xd * xd - ftwo * x * xdd)) / (ftwo * x)
            else
              dt_impact3
      else
        dt_impact in
  let impact_time := t + dt_impact2 in
  let events' := schedule_event sim1log.(sim_events) "flip_xd_at_bounce_event" impact_time in
  let result_sim := sim1log[[sim_events := events']] in
  let result_log := log_sim "differential_equations" result_sim in
  result_log.


