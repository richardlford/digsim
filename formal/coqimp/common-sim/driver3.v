Require Export Task.model_code.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import ListNotations.
Import RecordSetNotations'.
Open Scope D_scope.

Definition tenTo6 := (* Eval compute in *) strToFloat' "1.e6".

Definition process_one_event (ev: eventTy) (sim: simTy) :=
  let sim_log := log_sim "process_one_event:sim" sim in
  let vars := sim_log.(vars) in
  let t := svGetFloat SvT vars in
  let dt_min := svGetFloat SvDT_MIN vars in
  let dt_max := svGetFloat SvDT_MAX vars in
  let et := ev.(time) in
  let etdelta := round ((et - t) * tenTo6) in
  let minrnd := round (dt_min * tenTo6) in
  let (ev', result_sim) :=
      if (etdelta <? minrnd) then
        let (sim2, new_time_opt) := handle_event model_handlers ev sim_log in
        let sim2log := log_sim "process_one_event:sim2" sim2 in
        match new_time_opt with
        | Some new_time => (ev[[time := new_time]], sim2log)
        | None => (ev, sim2log)
        end
      else
        (ev, sim_log)
  in
  let result_log := log_sim "process_one_event" result_sim in
  (ev', result_log).

Fixpoint process_events_helper (evs: list eventTy) (sim: simTy) :=
  match evs with
  | nil => (nil, sim)
  | cons ev evtl =>
    let (ev', sim') := process_one_event ev sim in
    let (evs', sim'') := process_events_helper evtl sim' in
    (ev' :: evs', sim'')
  end.

Fixpoint min_event_time (evs: list eventTy) (min_so_far: float) : float :=
  match evs with
  | nil => min_so_far
  | cons ev evtl =>
    let et := ev.(time) in
    let new_min :=
        if (et >? 0%D) && (et <? min_so_far) then
          et
        else
          min_so_far
    in
    min_event_time evtl new_min
  end.

  
Definition process_events (sim: simTy) : simTy :=
  let sim_log := log_sim "process_events:sim" sim in
  let vars0 := sim_log.(vars) in
  let events := sim_log.(sim_events) in
  let result_sim := 
      match events with
      | nil => set_var SvDT (svGetFloat SvDT_MAX vars0) sim_log
      | cons x x0 =>
        let t := svGetFloat SvT vars0 in
        let dt_max := svGetFloat SvDT_MAX vars0 in
        let (evs', sim1) := process_events_helper events sim_log in
        let sim1log := log_sim "process_events:sim1" sim1 in
        let vars' := sim1log.(vars) in
        let min_time0 := svGetFloat SvT_STOP vars' in
        let min_time := min_event_time evs' min_time0 in
        let time_to_next_event := min_time - t in
        let new_dt :=
            if (time_to_next_event >? 0%D) && ((dt_max - time_to_next_event) >? epsilon) then
              time_to_next_event
            else 
              dt_max
        in
        let sim3 := set_var SvDT new_dt sim1log in
        let sim4 := sim3[[sim_events := evs']] in
        let sim4log := log_sim "process_events:sim4" sim4 in
        sim4log
      end in
  let result_log := log_sim "process_events:result" result_sim in
  result_log.

Fixpoint advance_states (pairs: list (stateVar * stateVar)) (dt: float) (sim: simTy) : simTy :=
  let sim_log := log_sim "advance_states:sim" sim in
  let result_sim := 
      match pairs with
      | nil => sim
      | cons (sv, svd) pairstl =>
        let vars0 := sim.(vars) in
        let el := svGetFloat sv vars0 in
        let eld := svGetFloat svd vars0 in
        let el' := el + eld * dt in
        let sim1 := set_var sv el' sim in
        let sim1log := log_sim "advance_states:sim1" sim1 in
        let sim2 := advance_states pairstl dt sim1log in
        sim2
      end in
  let result_log := log_sim "advance_states:result" result_sim in
  result_log.

Definition advance_model (sim: simTy) : simTy :=
  let sim_log := log_sim "advance_model:sim" sim in
  let vars0 := sim_log.(vars) in
  let dt := svGetFloat SvDT vars0 in
  let pairs := [(SvX, SvXD); (SvXD, SvXDD) ] in
  let sim2 := advance_states pairs dt sim_log in
  let sim2log := log_sim "advance_model:sim2" sim2 in
  let vars' := sim2log.(vars) in
  let t := svGetFloat SvT vars' in
  let new_t := t + dt in
  let rounded_new_t := round (new_t * tenTo6)/tenTo6 in
  (* let rounded_new_t := new_t in *)
  let result_sim := set_var SvT rounded_new_t sim2log in
  let result_log := log_sim "advance_model:result" result_sim in
  result_log.

Definition oneStep (sim: simTy) : simTy :=
  let sim_log := log_sim "oneStep:sim" sim in
  let sim1 := differential_equations sim_log in
  let sim1log := log_sim "oneStep:sim1" sim1 in
  let sim2 := process_events sim1log in
  let sim2log := log_sim "oneStep:sim2" sim2 in
  let sim3 :=
      if sim2log.(flags).(evaluate_xd) then
        let sim4 := differential_equations sim2log in
        let sim4log := log_sim "oneStep:sim4" sim4 in
        let flags4 := sim4log.(flags) in
        let flags4' := flags4[[evaluate_xd := false]] in
        let sim6 := sim4log[[flags := flags4']] in
        let sim6log := log_sim "oneStep:sim6" sim6 in
        sim6log
      else
        sim2log in
  let sim3log := log_sim "oneStep:sim3" sim3 in
  let sim5 := advance_model sim3log in
  let sim5log := log_sim "oneStep:sim5" sim5 in
  sim5log.

Require Import Zwf.
From compcert Require Import Coqlib.

Function run_sim_loop (steps: Z) (sim: simTy) { wf (Zwf 0%Z) steps } :=
  let result_sim := 
      if zle 0%Z steps
      then
        let f := sim.(flags) in
        if f.(end_of_run) || f.(stop_simulation)
        then sim
        else
          let sim2 := (oneStep sim) in
          (* let sim2log := log_sim "run_sim_loop:sim2" sim2 in *)
          let steps2 := (steps - 1) in 
          run_sim_loop steps2 sim2
      else sim
  in
  result_sim.
Proof.
  intros; red; omega.
  apply Zwf_well_founded.
Qed.


Definition run_sim (sim: simTy) :=
  let sim_log := log_sim "run_sim:sim" sim in
  let vars := sim_log.(vars) in
  let dtmin := svGetFloat SvDT_MIN vars in
  let tstop := svGetFloat SvT_STOP vars in
  let max_steps_float := (tstop / dtmin)%D in
  let steps := ZofFloat max_steps_float in
  let result_sim := run_sim_loop steps sim_log in
  let result_log := log_sim "run_sim:result" result_sim in
  result_log.

Definition sim_in (_ : unit) :=
  let sim1 := default_sim_log in
  let sim1log := log_sim "sim_in:sim1" sim1 in
  let sim2 := init_sim sim1log in
  let sim2log := log_sim "sim_in:sim2" sim2 in
  sim2log.

Definition main (_ : unit) :=
  let sim1 := (sim_in tt) in
  let sim1log := log_sim "sim_main:sim1" sim1 in
  let sim2 := run_sim  sim1 in
  let sim2log := log_sim "sim_main:result" sim2 in
  let sim3 := sim2log[[solution ::= (fun sol => rev sol)]] in
  let sim4 := sim3[[log ::= (fun lg => rev lg)]] in
  sim4.

