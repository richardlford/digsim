Require Export Task.model_code.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import ListNotations.
Import RecordSetNotations'.
Open Scope D_scope.

Definition tenTo6 := (* Eval compute in *) strToFloat' "1.e6".

Definition process_one_event (ev: eventTy) (sim: simTy) :=
  let vars := sim.(vars) in
  let t := svGetFloat SvT vars in
  let dt_min := svGetFloat SvDT_MIN vars in
  let dt_max := svGetFloat SvDT_MAX vars in
  let et := ev.(time) in
  let etdelta := round ((et - t) * tenTo6) in
  let minrnd := round (dt_min * tenTo6) in
  if (etdelta <? minrnd) then
    let (sim2, new_time_opt) := handle_event model_handlers ev sim in
    match new_time_opt with
    | Some new_time => (ev[[time := new_time]], sim2)
    | None => (ev, sim2)
    end
  else
    (ev, sim).

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
  let vars0 := sim.(vars) in
  let events := sim.(sim_events) in
  match events with
  | nil => set_var SvDT (svGetFloat SvDT_MAX vars0) sim
  | cons x x0 =>
    let t := svGetFloat SvT vars0 in
    let dt_max := svGetFloat SvDT_MAX vars0 in
    let (evs', sim1) := process_events_helper events sim in
    let vars' := sim1.(vars) in
    let min_time0 := svGetFloat SvT_STOP vars' in
    let min_time := min_event_time evs' min_time0 in
    let time_to_next_event := min_time - t in
    let new_dt :=
        if (time_to_next_event >? 0%D) && ((dt_max - time_to_next_event) >? epsilon) then
          time_to_next_event
        else 
          dt_max
    in
    let sim3 := set_var SvDT new_dt sim1 in
    sim3[[sim_events := evs']]
  end.

Fixpoint advance_states (pairs: list (stateVar * stateVar)) (dt: float) (sim: simTy) : simTy :=
  match pairs with
  | nil => sim
  | cons (sv, svd) pairstl =>
    let vars0 := sim.(vars) in
    let el := svGetFloat sv vars0 in
    let eld := svGetFloat svd vars0 in
    let el' := el + eld * dt in
    let sim1 := set_var sv el' sim in
    advance_states pairstl dt sim1
  end.

Definition advance_model (sim: simTy) : simTy :=
  let vars0 := sim.(vars) in
  let dt := svGetFloat SvDT vars0 in
  let sim2 := advance_states modelPairs dt sim in
  let vars' := sim2.(vars) in
  let t := svGetFloat SvT vars' in
  let new_t := t + dt in
  let rounded_new_t := round (new_t * tenTo6)/tenTo6 in
  set_var SvT rounded_new_t sim2.

Definition oneStep (sim: simTy) : simTy :=
  let sim1 := differential_equations sim in
  let sim2 := process_events sim1 in
  let sim3 :=
      if sim2.(flags).(evaluate_xd) then
        let sim4 := differential_equations sim2 in
        let flags4 := sim4.(flags) in
        let flags4' := flags4[[evaluate_xd := false]] in
        sim4[[flags := flags4']]
      else
        sim2 in
  advance_model sim3.

Require Import Zwf.
From compcert Require Import Coqlib.

Function run_sim_loop (steps: Z) (sim: simTy) { wf (Zwf 0%Z) steps } :=
  if zle 0%Z steps
  then
    let f := sim.(flags) in
    if f.(end_of_run) || f.(stop_simulation)
    then sim
    else
      let sim2 := (oneStep sim) in
      let steps2 := (steps - 1) in 
      run_sim_loop steps2 sim2
  else sim.
Proof.
  intros; red; omega.
  apply Zwf_well_founded.
Qed.

Definition run_sim (sim: simTy) :=
  let vars := sim.(vars) in
  let dtmin := svGetFloat SvDT_MIN vars in
  let tstop := svGetFloat SvT_STOP vars in
  let max_steps_float := (tstop / dtmin)%D in
  let steps := ZofFloat max_steps_float in
  run_sim_loop steps sim.

Definition main (_ : unit) :=
  let sim1 := init_sim default_sim in
  let sim2 := run_sim sim1 in
  let sim3 := sim2[[solution ::= (fun sol => rev sol)]] in
  sim3[[log ::= (fun lg => rev lg)]].


