open Floats
open Driver2
open Model_data

(** val model_handlers : (char list * event_function_signature) list **)

let model_handlers =
  driver_default_handlers

(** val init_sim : simTy -> simTy **)

let init_sim sim =
  log_sim ('i'::('n'::('i'::('t'::('_'::('s'::('i'::('m'::(':'::('s'::('i'::('m'::[])))))))))))) sim

(** val differential_equations : simTy -> simTy **)

let differential_equations sim =
  let sim_log =
    log_sim
      ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::[]))))))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let x = svGetFloat SvX vars0 in
  let xd = svGetFloat SvXD vars0 in
  let gravity = svGetFloat SvGRAVITY vars0 in
  let damping_coefficient = svGetFloat SvDampingCoefficient vars0 in
  let spring_coefficient = svGetFloat SvSpringCoefficient vars0 in
  let mass = svGetFloat SvMass vars0 in
  let xdd =
    Float.sub
      (Float.div (Float.neg (Float.add (Float.mul spring_coefficient x) (Float.mul damping_coefficient xd)))
        mass) gravity
  in
  let sim1 = set_var SvXDD xdd sim_log in
  log_sim
    ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))
    sim1
