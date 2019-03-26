open Floats
open Integers
open RecordSet
open Debug_printers
open Driver2
open Float_text_io
open Model_data

(** val z_terminate_sim_event : eventTy **)

let z_terminate_sim_event =
  { key =
    ('z'::('_'::('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))))));
    time = Float.zero }

(** val sq : float -> float **)

let sq f =
  Float.mul f f

(** val log_miss : simTy -> simTy **)

let log_miss sim =
  let vars0 = sim.vars in
  let t = svGetFloat SvT vars0 in
  let x = svGetFloat SvX vars0 in
  let xd = svGetFloat SvXD vars0 in
  let z = svGetFloat SvZ vars0 in
  let zd = svGetFloat SvZD vars0 in
  let dt_miss =
    Float.div (Float.add (Float.mul x xd) (Float.mul z zd))
      (Float.add (sq xd) (sq zd))
  in
  let r_miss =
    FloatIO.sqrt
      (Float.add (sq (Float.sub x (Float.mul xd dt_miss)))
        (sq (Float.sub z (Float.mul zd dt_miss))))
  in
  let miss_vars = (('t'::[]),
    (DebugIO.print_float t)) :: ((('r'::('_'::('m'::('i'::('s'::('s'::[])))))),
    (DebugIO.print_float r_miss)) :: ((('d'::('t'::('_'::('m'::('i'::('s'::('s'::[]))))))),
    (DebugIO.print_float dt_miss)) :: []))
  in
  let miss_entry = { le_caption =
    ('l'::('o'::('g'::('_'::('m'::('i'::('s'::('s'::[])))))))); le_vars =
    miss_vars; le_events = [] }
  in
  set log (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
    e.solution; sim_events = e.sim_events; log = (f e.log); flags =
    e.flags }) (fun oldlog -> miss_entry :: oldlog) sim

(** val z_terminate_sim_event_func : event_function_signature **)

let z_terminate_sim_event_func _ sim =
  let vars0 = sim.vars in
  let z = svGetFloat SvZ vars0 in
  let result_sim =
    if Float.cmp Cge z Float.zero
    then let sim1 = log_miss sim in
         let flags2 =
           set end_of_run (fun f e -> { stop_simulation = e.stop_simulation;
             end_of_run = (f e.end_of_run); evaluate_xd = e.evaluate_xd })
             (constructor true) sim1.flags
         in
         set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys;
           solution = e.solution; sim_events = e.sim_events; log = e.log;
           flags = (f e.flags) }) (constructor flags2) sim1
    else sim
  in
  (result_sim, None)

(** val model_handlers : (char list * event_function_signature) list **)

let model_handlers =
  (('z'::('_'::('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))))))))),
    z_terminate_sim_event_func) :: driver_default_handlers

(** val init_sim : simTy -> simTy **)

let init_sim sim =
  let sim1 =
    log_sim
      ('i'::('n'::('i'::('t'::('_'::('s'::('i'::('m'::(':'::('s'::('i'::('m'::[]))))))))))))
      sim
  in
  set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
    e.solution; sim_events = (f e.sim_events); log = e.log; flags =
    e.flags }) (fun others -> z_terminate_sim_event :: others) sim1

(** val sin : float -> float **)

let sin f =
  f

(** val cos : float -> float **)

let cos f =
  f

(** val differential_equations : simTy -> simTy **)

let differential_equations sim =
  let vars0 = sim.vars in
  let x = svGetFloat SvX vars0 in
  let z = svGetFloat SvZ vars0 in
  let theta = svGetFloat SvTHETA vars0 in
  let velocity = svGetFloat SvVELOCITY vars0 in
  let guidance_gain = svGetFloat SvGUIDANCE_GAIN vars0 in
  let xd = Float.mul velocity (cos theta) in
  let zd = Float.mul (Float.neg velocity) (sin theta) in
  let q_s =
    Float.div (Float.sub (Float.mul z xd) (Float.mul x zd))
      (Float.add (Float.mul x x) (Float.mul z z))
  in
  let theta_dot_cmd = Float.mul guidance_gain q_s in
  union_vars sim ((SvXD, xd) :: ((SvZD, zd) :: ((SvTHETA_DOT,
    theta_dot_cmd) :: ((SvTHETA_DOT_CMD, theta_dot_cmd) :: ((SvQ_S,
    q_s) :: ((SvQ_S_MEAS, q_s) :: []))))))
