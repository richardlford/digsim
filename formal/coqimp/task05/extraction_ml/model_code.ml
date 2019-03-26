open Floats
open Integers
open RecordSet
open Driver2
open Float_text_io
open Model_data

(** val fhalf : float **)

let fhalf =
  FloatIO.strToFloat' ('0'::('.'::('5'::[])))

(** val fone : float **)

let fone =
  FloatIO.strToFloat' ('1'::('.'::('0'::[])))

(** val ftwo : float **)

let ftwo =
  FloatIO.strToFloat' ('2'::('.'::('0'::[])))

(** val f99 : float **)

let f99 =
  FloatIO.strToFloat' ('9'::('9'::('.'::('0'::[]))))

(** val bounceEvent : eventTy **)

let bounceEvent =
  { key =
    ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))))))));
    time = f99 }

(** val flip_xd_at_bounce_event_func :
    eventTy -> simTy -> simTy * float option **)

let flip_xd_at_bounce_event_func _ sim =
  let sim' =
    log_sim
      ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('s'::('i'::('m'::[])))))))))))))))))))))))))))))))))
      sim
  in
  let vars0 = sim'.vars in
  let coeff_of_rest = svGetFloat SvCOEFF_OF_REST vars0 in
  let xd = svGetFloat SvXD vars0 in
  let new_xd = Float.neg (Float.mul coeff_of_rest xd) in
  let vars' = SvTree.set SvXD new_xd vars0 in
  let new_flags =
    set evaluate_xd (fun f e -> { stop_simulation = e.stop_simulation;
      end_of_run = e.end_of_run; evaluate_xd = (f e.evaluate_xd) })
      (constructor true) sim'.flags
  in
  let sim2 = set_vars sim' vars' in
  let result_sim =
    set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
      e.solution; sim_events = e.sim_events; log = e.log; flags =
      (f e.flags) }) (constructor new_flags) sim2
  in
  let result_log =
    log_sim
      ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::[]))))))))))))))))))))))))))))
      result_sim
  in
  (result_log, (Some f99))

(** val model_handlers :
    (char list * (eventTy -> simTy -> simTy * float option)) list **)

let model_handlers =
  (('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))))))))))),
    flip_xd_at_bounce_event_func) :: driver_default_handlers

(** val init_sim : simTy -> simTy **)

let init_sim sim =
  let result_sim =
    set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys;
      solution = e.solution; sim_events = (f e.sim_events); log = e.log;
      flags = e.flags }) (fun evs -> bounceEvent :: evs) sim
  in
  log_sim
    ('i'::('n'::('i'::('t'::('_'::('s'::('i'::('m'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))))
    result_sim

(** val differential_equations : simTy -> simTy **)

let differential_equations sim =
  let sim_log =
    log_sim
      ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::[]))))))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let t = svGetFloat SvT vars0 in
  let x = svGetFloat SvX vars0 in
  let xd = svGetFloat SvXD vars0 in
  let dt_min = svGetFloat SvDT_MIN vars0 in
  let dt_max = svGetFloat SvDT_MAX vars0 in
  let gravity = svGetFloat SvGRAVITY vars0 in
  let t_stop = svGetFloat SvT_STOP vars0 in
  let xdd = Float.neg gravity in
  let sim1 = set_var SvXDD xdd sim_log in
  let sim1log =
    log_sim
      ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))
      sim1
  in
  let est_max =
    Float.add (Float.add x (Float.mul xd dt_max))
      (Float.mul (Float.mul (Float.mul fhalf xdd) dt_max) dt_max)
  in
  let dt_impact = Float.add t_stop fone in
  let dt_impact2 =
    if Float.cmp Cle est_max Float.zero
    then let est_min =
           Float.add (Float.add x (Float.mul xd dt_min))
             (Float.mul (Float.mul (Float.mul fhalf xdd) dt_min) dt_min)
         in
         if Float.cmp Cle est_min Float.zero
         then Float.zero
         else let dt_impact3 =
                Float.div
                  (Float.sub (Float.neg xd)
                    (FloatIO.sqrt
                      (Float.sub (Float.mul xd xd)
                        (Float.mul (Float.mul ftwo x) xdd))))
                  (Float.mul ftwo x)
              in
              if Float.cmp Cgt (Float.sub dt_min dt_impact3) epsilon
              then Float.div
                     (Float.add (Float.neg xd)
                       (FloatIO.sqrt
                         (Float.sub (Float.mul xd xd)
                           (Float.mul (Float.mul ftwo x) xdd))))
                     (Float.mul ftwo x)
              else dt_impact3
    else dt_impact
  in
  let impact_time = Float.add t dt_impact2 in
  let events' =
    schedule_event sim1log.sim_events
      ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))))))))
      impact_time
  in
  let result_sim =
    set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys;
      solution = e.solution; sim_events = (f e.sim_events); log = e.log;
      flags = e.flags }) (constructor events') sim1log
  in
  log_sim
    ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::[]))))))))))))))))))))))
    result_sim
