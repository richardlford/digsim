Require Export Task.driver2.

Import ListNotations.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import RecordSetNotations'.
Open Scope D_scope.

Definition model_handlers := driver_default_handlers.

Definition init_sim (sim: simTy) :=
  let sim' := log_sim "init_sim:sim" sim in
  sim'.

Definition differential_equations (sim: simTy) : simTy :=
  let sim_log := log_sim "differential_equations" sim in
  let vars := sim_log.(vars) in
  let t := svGetFloat SvT vars in
  let x := svGetFloat SvX vars in
  let xd := svGetFloat SvXD vars in
  let gravity := svGetFloat SvGRAVITY vars in
  let damping_coefficient := svGetFloat SvDampingCoefficient vars in
  let spring_coefficient := svGetFloat SvSpringCoefficient vars in
  let mass := svGetFloat SvMass vars in
  let xdd := (- (spring_coefficient * x + damping_coefficient * xd)) / mass - gravity in
  let sim1 := set_var SvXDD xdd sim_log in
  let sim1log := log_sim "differential_equations:sim1" sim1 in
  sim1log.
