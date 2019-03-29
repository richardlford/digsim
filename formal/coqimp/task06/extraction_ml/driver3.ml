open BinInt
open BinNums
open Coqlib
open Floats
open Integers
open List0
open RecordSet
open Driver2
open Float_text_io
open Model_code
open Model_data

(** val tenTo6 : float **)

let tenTo6 =
  FloatIO.strToFloat' ('1'::('.'::('e'::('6'::[]))))

(** val process_one_event : eventTy -> simTy -> eventTy * simTy **)

let process_one_event ev sim =
  let sim_log =
    log_sim
      ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('o'::('n'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::(':'::('s'::('i'::('m'::[])))))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let t = svGetFloat SvT vars0 in
  let dt_min = svGetFloat SvDT_MIN vars0 in
  let et = ev.time in
  let etdelta = FloatIO.round (Float.mul (Float.sub et t) tenTo6) in
  let minrnd = FloatIO.round (Float.mul dt_min tenTo6) in
  let (ev', result_sim) =
    if Float.cmp Clt etdelta minrnd
    then let (sim2, new_time_opt) = handle_event model_handlers ev sim_log in
         let sim2log =
           log_sim
             ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('o'::('n'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::(':'::('s'::('i'::('m'::('2'::[]))))))))))))))))))))))
             sim2
         in
         (match new_time_opt with
          | Some new_time ->
            ((set time (fun f e -> { key = e.key; time = (f e.time) })
               (constructor new_time) ev), sim2log)
          | None -> (ev, sim2log))
    else (ev, sim_log)
  in
  let result_log =
    log_sim
      ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('o'::('n'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))
      result_sim
  in
  (ev', result_log)

(** val process_events_helper :
    eventTy list -> simTy -> eventTy list * simTy **)

let rec process_events_helper evs sim =
  match evs with
  | [] -> ([], sim)
  | ev :: evtl ->
    let (ev', sim') = process_one_event ev sim in
    let (evs', sim'') = process_events_helper evtl sim' in
    ((ev' :: evs'), sim'')

(** val min_event_time : eventTy list -> float -> float **)

let rec min_event_time evs min_so_far =
  match evs with
  | [] -> min_so_far
  | ev :: evtl ->
    let et = ev.time in
    let new_min =
      if (&&) (Float.cmp Cgt et Float.zero) (Float.cmp Clt et min_so_far)
      then et
      else min_so_far
    in
    min_event_time evtl new_min

(** val process_events : simTy -> simTy **)

let process_events sim =
  let sim_log =
    log_sim
      ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('s'::('i'::('m'::[]))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let events = sim_log.sim_events in
  let result_sim =
    match events with
    | [] -> set_var SvDT (svGetFloat SvDT_MAX vars0) sim_log
    | _ :: _ ->
      let t = svGetFloat SvT vars0 in
      let dt_max = svGetFloat SvDT_MAX vars0 in
      let (evs', sim1) = process_events_helper events sim_log in
      let sim1log =
        log_sim
          ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))
          sim1
      in
      let vars' = sim1log.vars in
      let min_time0 = svGetFloat SvT_STOP vars' in
      let min_time = min_event_time evs' min_time0 in
      let time_to_next_event = Float.sub min_time t in
      let new_dt =
        if (&&) (Float.cmp Cgt time_to_next_event Float.zero)
             (Float.cmp Cgt (Float.sub dt_max time_to_next_event) epsilon)
        then time_to_next_event
        else dt_max
      in
      let sim3 = set_var SvDT new_dt sim1log in
      let sim4 =
        set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys;
          solution = e.solution; sim_events = (f e.sim_events); log = e.log;
          flags = e.flags }) (constructor evs') sim3
      in
      log_sim
        ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('s'::('i'::('m'::('4'::[])))))))))))))))))))
        sim4
  in
  log_sim
    ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))))
    result_sim

(** val advance_states :
    (stateVar * stateVar) list -> float -> simTy -> simTy **)

let rec advance_states pairs dt sim =
  let result_sim =
    match pairs with
    | [] -> sim
    | p :: pairstl ->
      let (sv, svd) = p in
      let vars0 = sim.vars in
      let el = svGetFloat sv vars0 in
      let eld = svGetFloat svd vars0 in
      let el' = Float.add el (Float.mul eld dt) in
      let sim1 = set_var sv el' sim in
      let sim1log =
        log_sim
          ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('s'::('t'::('a'::('t'::('e'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))
          sim1
      in
      advance_states pairstl dt sim1log
  in
  log_sim
    ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('s'::('t'::('a'::('t'::('e'::('s'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))))
    result_sim

(** val advance_model : simTy -> simTy **)

let advance_model sim =
  let sim_log =
    log_sim
      ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('m'::('o'::('d'::('e'::('l'::(':'::('s'::('i'::('m'::[])))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let dt = svGetFloat SvDT vars0 in
  let sim2 = advance_states modelPairs dt sim_log in
  let sim2log =
    log_sim
      ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('m'::('o'::('d'::('e'::('l'::(':'::('s'::('i'::('m'::('2'::[]))))))))))))))))))
      sim2
  in
  let vars' = sim2log.vars in
  let t = svGetFloat SvT vars' in
  let new_t = Float.add t dt in
  let rounded_new_t =
    Float.div (FloatIO.round (Float.mul new_t tenTo6)) tenTo6
  in
  let result_sim = set_var SvT rounded_new_t sim2log in
  log_sim
    ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('m'::('o'::('d'::('e'::('l'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))))))))
    result_sim

(** val oneStep : simTy -> simTy **)

let oneStep sim =
  let sim_log =
    log_sim
      ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::[])))))))))))
      sim
  in
  let sim1 = differential_equations sim_log in
  let sim1log =
    log_sim
      ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('1'::[]))))))))))))
      sim1
  in
  let sim2 = process_events sim1log in
  let sim2log =
    log_sim
      ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('2'::[]))))))))))))
      sim2
  in
  let sim3 =
    if sim2log.flags.evaluate_xd
    then let sim4 = differential_equations sim2log in
         let sim4log =
           log_sim
             ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('4'::[]))))))))))))
             sim4
         in
         let flags4 = sim4log.flags in
         let flags4' =
           set evaluate_xd (fun f e -> { stop_simulation = e.stop_simulation;
             end_of_run = e.end_of_run; evaluate_xd = (f e.evaluate_xd) })
             (constructor false) flags4
         in
         let sim6 =
           set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys;
             solution = e.solution; sim_events = e.sim_events; log = e.log;
             flags = (f e.flags) }) (constructor flags4') sim4log
         in
         log_sim
           ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('6'::[]))))))))))))
           sim6
    else sim2log
  in
  let sim3log =
    log_sim
      ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('3'::[]))))))))))))
      sim3
  in
  let sim5 = advance_model sim3log in
  log_sim
    ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('5'::[]))))))))))))
    sim5

(** val run_sim_loop : coq_Z -> simTy -> simTy **)

let rec run_sim_loop steps sim =
  if zle Z0 steps
  then if (||) sim.flags.end_of_run sim.flags.stop_simulation
       then sim
       else run_sim_loop (Z.sub steps (Zpos Coq_xH)) (oneStep sim)
  else sim

(** val run_sim : simTy -> simTy **)

let run_sim sim =
  let sim_log =
    log_sim
      ('r'::('u'::('n'::('_'::('s'::('i'::('m'::(':'::('s'::('i'::('m'::[])))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let dtmin = svGetFloat SvDT_MIN vars0 in
  let tstop = svGetFloat SvT_STOP vars0 in
  let max_steps_float = Float.div tstop dtmin in
  let steps = FloatIO.coq_ZofFloat max_steps_float in
  let result_sim = run_sim_loop steps sim_log in
  log_sim
    ('r'::('u'::('n'::('_'::('s'::('i'::('m'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))
    result_sim

(** val sim_in : unit -> simTy **)

let sim_in _ =
  let sim1log =
    log_sim
      ('s'::('i'::('m'::('_'::('i'::('n'::(':'::('s'::('i'::('m'::('1'::[])))))))))))
      default_sim_log
  in
  let sim2 = init_sim sim1log in
  log_sim
    ('s'::('i'::('m'::('_'::('i'::('n'::(':'::('s'::('i'::('m'::('2'::[])))))))))))
    sim2

(** val main : unit -> simTy **)

let main _ =
  let sim1 = sim_in () in
  let sim2 = run_sim sim1 in
  let sim2log =
    log_sim
      ('s'::('i'::('m'::('_'::('m'::('a'::('i'::('n'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))
      sim2
  in
  let sim3 =
    set solution (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
      (f e.solution); sim_events = e.sim_events; log = e.log; flags =
      e.flags }) rev sim2log
  in
  set log (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
    e.solution; sim_events = e.sim_events; log = (f e.log); flags =
    e.flags }) rev sim3
