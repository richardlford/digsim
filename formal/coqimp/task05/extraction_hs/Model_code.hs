module Model_code where

import qualified Prelude
import qualified Floats
import qualified Integers
import qualified RecordSet
import qualified Driver2
import qualified Float_text_io
import qualified Model_data

fhalf :: Floats.Coq_float
fhalf =
  Float_text_io._FloatIO__strToFloat' ((:) '0' ((:) '.' ((:) '5' ([]))))

fone :: Floats.Coq_float
fone =
  Float_text_io._FloatIO__strToFloat' ((:) '1' ((:) '.' ((:) '0' ([]))))

ftwo :: Floats.Coq_float
ftwo =
  Float_text_io._FloatIO__strToFloat' ((:) '2' ((:) '.' ((:) '0' ([]))))

f99 :: Floats.Coq_float
f99 =
  Float_text_io._FloatIO__strToFloat' ((:) '9' ((:) '9' ((:) '.' ((:) '0' ([])))))

bounceEvent :: Driver2.Coq_eventTy
bounceEvent =
  Driver2.Coq_mkEvent ((:) 'f' ((:) 'l' ((:) 'i' ((:) 'p' ((:) '_' ((:) 'x' ((:) 'd' ((:) '_' ((:)
    'a' ((:) 't' ((:) '_' ((:) 'b' ((:) 'o' ((:) 'u' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_' ((:) 'e'
    ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ([])))))))))))))))))))))))) f99

flip_xd_at_bounce_event_func :: Driver2.Coq_eventTy -> Driver2.Coq_simTy -> (,) Driver2.Coq_simTy
                                (Prelude.Maybe Floats.Coq_float)
flip_xd_at_bounce_event_func _ sim =
  let {
   sim' = Driver2.log_sim ((:) 'f' ((:) 'l' ((:) 'i' ((:) 'p' ((:) '_' ((:) 'x' ((:) 'd' ((:) '_'
            ((:) 'a' ((:) 't' ((:) '_' ((:) 'b' ((:) 'o' ((:) 'u' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_'
            ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) '_' ((:) 'f' ((:) 'u' ((:) 'n' ((:) 'c'
            ((:) ':' ((:) ' ' ((:) 's' ((:) 'i' ((:) 'm' ([])))))))))))))))))))))))))))))))))) sim}
  in
  let {vars0 = Driver2.vars sim'} in
  let {coeff_of_rest = Driver2.svGetFloat Model_data.SvCOEFF_OF_REST vars0} in
  let {xd = Driver2.svGetFloat Model_data.SvXD vars0} in
  let {new_xd = Floats._Float__neg (Floats._Float__mul coeff_of_rest xd)} in
  let {vars' = Driver2._SvTree__set Model_data.SvXD new_xd vars0} in
  let {
   new_flags = RecordSet.set Driver2.evaluate_xd (\f e -> Driver2.Coq_mkFlags
                 (Driver2.stop_simulation e) (Driver2.end_of_run e) (f (Driver2.evaluate_xd e)))
                 (RecordSet.constructor Prelude.True) (Driver2.flags sim')}
  in
  let {sim2 = Driver2.set_vars sim' vars'} in
  let {
   result_sim = RecordSet.set Driver2.flags (\f e -> Driver2.Coq_mkSim (Driver2.vars e)
                  (Driver2.solkeys e) (Driver2.solution e) (Driver2.sim_events e) (Driver2.log e)
                  (f (Driver2.flags e))) (RecordSet.constructor new_flags) sim2}
  in
  let {
   result_log = Driver2.log_sim ((:) 'f' ((:) 'l' ((:) 'i' ((:) 'p' ((:) '_' ((:) 'x' ((:) 'd' ((:)
                  '_' ((:) 'a' ((:) 't' ((:) '_' ((:) 'b' ((:) 'o' ((:) 'u' ((:) 'n' ((:) 'c' ((:)
                  'e' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) '_' ((:) 'f' ((:)
                  'u' ((:) 'n' ((:) 'c' ([]))))))))))))))))))))))))))))) result_sim}
  in
  (,) result_log (Prelude.Just f99)

model_handlers :: ([])
                  ((,) Prelude.String
                  (Driver2.Coq_eventTy -> Driver2.Coq_simTy -> (,) Driver2.Coq_simTy
                  (Prelude.Maybe Floats.Coq_float)))
model_handlers =
  (:) ((,) ((:) 'f' ((:) 'l' ((:) 'i' ((:) 'p' ((:) '_' ((:) 'x' ((:) 'd' ((:) '_' ((:) 'a' ((:) 't'
    ((:) '_' ((:) 'b' ((:) 'o' ((:) 'u' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_' ((:) 'e' ((:) 'v' ((:)
    'e' ((:) 'n' ((:) 't' ([])))))))))))))))))))))))) flip_xd_at_bounce_event_func)
    Driver2.driver_default_handlers

init_sim :: Driver2.Coq_simTy -> Driver2.Coq_simTy
init_sim sim =
  let {
   result_sim = RecordSet.set Driver2.sim_events (\f e -> Driver2.Coq_mkSim (Driver2.vars e)
                  (Driver2.solkeys e) (Driver2.solution e) (f (Driver2.sim_events e)) (Driver2.log e)
                  (Driver2.flags e)) (\evs -> (:) bounceEvent evs) sim}
  in
  Driver2.log_sim ((:) 'i' ((:) 'n' ((:) 'i' ((:) 't' ((:) '_' ((:) 's' ((:) 'i' ((:) 'm' ((:) ':'
    ((:) ' ' ((:) 'r' ((:) 'e' ((:) 's' ((:) 'u' ((:) 'l' ((:) 't' ([]))))))))))))))))) result_sim

differential_equations :: Driver2.Coq_simTy -> Driver2.Coq_simTy
differential_equations sim =
  let {
   sim_log = Driver2.log_sim ((:) 'd' ((:) 'i' ((:) 'f' ((:) 'f' ((:) 'e' ((:) 'r' ((:) 'e' ((:) 'n'
               ((:) 't' ((:) 'i' ((:) 'a' ((:) 'l' ((:) '_' ((:) 'e' ((:) 'q' ((:) 'u' ((:) 'a' ((:)
               't' ((:) 'i' ((:) 'o' ((:) 'n' ((:) 's' ([]))))))))))))))))))))))) sim}
  in
  let {vars0 = Driver2.vars sim_log} in
  let {t = Driver2.svGetFloat Model_data.SvT vars0} in
  let {x = Driver2.svGetFloat Model_data.SvX vars0} in
  let {xd = Driver2.svGetFloat Model_data.SvXD vars0} in
  let {dt_min = Driver2.svGetFloat Model_data.SvDT_MIN vars0} in
  let {dt_max = Driver2.svGetFloat Model_data.SvDT_MAX vars0} in
  let {gravity = Driver2.svGetFloat Model_data.SvGRAVITY vars0} in
  let {t_stop = Driver2.svGetFloat Model_data.SvT_STOP vars0} in
  let {xdd = Floats._Float__neg gravity} in
  let {sim1 = Driver2.set_var Model_data.SvXDD xdd sim_log} in
  let {
   sim1log = Driver2.log_sim ((:) 'd' ((:) 'i' ((:) 'f' ((:) 'f' ((:) 'e' ((:) 'r' ((:) 'e' ((:) 'n'
               ((:) 't' ((:) 'i' ((:) 'a' ((:) 'l' ((:) '_' ((:) 'e' ((:) 'q' ((:) 'u' ((:) 'a' ((:)
               't' ((:) 'i' ((:) 'o' ((:) 'n' ((:) 's' ((:) ':' ((:) 's' ((:) 'i' ((:) 'm' ((:) '1'
               ([])))))))))))))))))))))))))))) sim1}
  in
  let {
   est_max = Floats._Float__add (Floats._Float__add x (Floats._Float__mul xd dt_max))
               (Floats._Float__mul (Floats._Float__mul (Floats._Float__mul fhalf xdd) dt_max) dt_max)}
  in
  let {dt_impact = Floats._Float__add t_stop fone} in
  let {
   dt_impact2 = case Floats._Float__cmp Integers.Cle est_max Floats._Float__zero of {
                 Prelude.True ->
                  let {
                   est_min = Floats._Float__add (Floats._Float__add x (Floats._Float__mul xd dt_min))
                               (Floats._Float__mul
                                 (Floats._Float__mul (Floats._Float__mul fhalf xdd) dt_min) dt_min)}
                  in
                  case Floats._Float__cmp Integers.Cle est_min Floats._Float__zero of {
                   Prelude.True -> Floats._Float__zero;
                   Prelude.False ->
                    let {
                     dt_impact3 = Floats._Float__div
                                    (Floats._Float__sub (Floats._Float__neg xd)
                                      (Float_text_io._FloatIO__sqrt
                                        (Floats._Float__sub (Floats._Float__mul xd xd)
                                          (Floats._Float__mul (Floats._Float__mul ftwo x) xdd))))
                                    (Floats._Float__mul ftwo x)}
                    in
                    case Floats._Float__cmp Integers.Cgt (Floats._Float__sub dt_min dt_impact3)
                           Driver2.epsilon of {
                     Prelude.True ->
                      Floats._Float__div
                        (Floats._Float__add (Floats._Float__neg xd)
                          (Float_text_io._FloatIO__sqrt
                            (Floats._Float__sub (Floats._Float__mul xd xd)
                              (Floats._Float__mul (Floats._Float__mul ftwo x) xdd))))
                        (Floats._Float__mul ftwo x);
                     Prelude.False -> dt_impact3}};
                 Prelude.False -> dt_impact}}
  in
  let {impact_time = Floats._Float__add t dt_impact2} in
  let {
   events' = Driver2.schedule_event (Driver2.sim_events sim1log) ((:) 'f' ((:) 'l' ((:) 'i' ((:) 'p'
               ((:) '_' ((:) 'x' ((:) 'd' ((:) '_' ((:) 'a' ((:) 't' ((:) '_' ((:) 'b' ((:) 'o' ((:)
               'u' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't'
               ([])))))))))))))))))))))))) impact_time}
  in
  let {
   result_sim = RecordSet.set Driver2.sim_events (\f e -> Driver2.Coq_mkSim (Driver2.vars e)
                  (Driver2.solkeys e) (Driver2.solution e) (f (Driver2.sim_events e)) (Driver2.log e)
                  (Driver2.flags e)) (RecordSet.constructor events') sim1log}
  in
  Driver2.log_sim ((:) 'd' ((:) 'i' ((:) 'f' ((:) 'f' ((:) 'e' ((:) 'r' ((:) 'e' ((:) 'n' ((:) 't'
    ((:) 'i' ((:) 'a' ((:) 'l' ((:) '_' ((:) 'e' ((:) 'q' ((:) 'u' ((:) 'a' ((:) 't' ((:) 'i' ((:)
    'o' ((:) 'n' ((:) 's' ([]))))))))))))))))))))))) result_sim

