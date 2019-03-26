module Driver3 where

import qualified Prelude
import qualified Coqlib
import qualified Floats
import qualified Integers
import qualified List0
import qualified Logic
import qualified RecordSet
import qualified Specif
import qualified Driver2
import qualified Float_text_io
import qualified Model_code
import qualified Model_data

tenTo6 :: Floats.Coq_float
tenTo6 =
  Float_text_io._FloatIO__strToFloat' ((:) '1' ((:) '.' ((:) 'e' ((:) '6' ([])))))

process_one_event :: Driver2.Coq_eventTy -> Driver2.Coq_simTy -> (,) Driver2.Coq_eventTy
                     Driver2.Coq_simTy
process_one_event ev sim =
  let {
   sim_log = Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's' ((:) 's' ((:) '_'
               ((:) 'o' ((:) 'n' ((:) 'e' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:)
               ':' ((:) 's' ((:) 'i' ((:) 'm' ([])))))))))))))))))))))) sim}
  in
  let {vars0 = Driver2.vars sim_log} in
  let {t = Driver2.svGetFloat Model_data.SvT vars0} in
  let {dt_min = Driver2.svGetFloat Model_data.SvDT_MIN vars0} in
  let {et = Driver2.time ev} in
  let {etdelta = Float_text_io._FloatIO__round (Floats._Float__mul (Floats._Float__sub et t) tenTo6)}
  in
  let {minrnd = Float_text_io._FloatIO__round (Floats._Float__mul dt_min tenTo6)} in
  case case Floats._Float__cmp Integers.Clt etdelta minrnd of {
        Prelude.True ->
         case Driver2.handle_event Model_code.model_handlers ev sim_log of {
          (,) sim2 new_time_opt ->
           let {
            sim2log = Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's' ((:) 's'
                        ((:) '_' ((:) 'o' ((:) 'n' ((:) 'e' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:)
                        'n' ((:) 't' ((:) ':' ((:) 's' ((:) 'i' ((:) 'm' ((:) '2'
                        ([]))))))))))))))))))))))) sim2}
           in
           case new_time_opt of {
            Prelude.Just new_time -> (,)
             (RecordSet.set Driver2.time (\f e -> Driver2.Coq_mkEvent (Driver2.key e)
               (f (Driver2.time e))) (RecordSet.constructor new_time) ev) sim2log;
            Prelude.Nothing -> (,) ev sim2log}};
        Prelude.False -> (,) ev sim_log} of {
   (,) ev' result_sim ->
    let {
     result_log = Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's' ((:) 's' ((:)
                    '_' ((:) 'o' ((:) 'n' ((:) 'e' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:)
                    't' ([])))))))))))))))))) result_sim}
    in
    (,) ev' result_log}

process_events_helper :: (([]) Driver2.Coq_eventTy) -> Driver2.Coq_simTy -> (,)
                         (([]) Driver2.Coq_eventTy) Driver2.Coq_simTy
process_events_helper evs sim =
  case evs of {
   ([]) -> (,) ([]) sim;
   (:) ev evtl ->
    case process_one_event ev sim of {
     (,) ev' sim' ->
      case process_events_helper evtl sim' of {
       (,) evs' sim'' -> (,) ((:) ev' evs') sim''}}}

min_event_time :: (([]) Driver2.Coq_eventTy) -> Floats.Coq_float -> Floats.Coq_float
min_event_time evs min_so_far =
  case evs of {
   ([]) -> min_so_far;
   (:) ev evtl ->
    let {et = Driver2.time ev} in
    let {
     new_min = case (Prelude.&&) (Floats._Float__cmp Integers.Cgt et Floats._Float__zero)
                      (Floats._Float__cmp Integers.Clt et min_so_far) of {
                Prelude.True -> et;
                Prelude.False -> min_so_far}}
    in
    min_event_time evtl new_min}

process_events :: Driver2.Coq_simTy -> Driver2.Coq_simTy
process_events sim =
  let {
   sim_log = Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's' ((:) 's' ((:) '_'
               ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:) ':' ((:) 's' ((:) 'i' ((:)
               'm' ([]))))))))))))))))))) sim}
  in
  let {vars0 = Driver2.vars sim_log} in
  let {events = Driver2.sim_events sim_log} in
  let {
   result_sim = case events of {
                 ([]) ->
                  Driver2.set_var Model_data.SvDT (Driver2.svGetFloat Model_data.SvDT_MAX vars0)
                    sim_log;
                 (:) _ _ ->
                  let {t = Driver2.svGetFloat Model_data.SvT vars0} in
                  let {dt_max = Driver2.svGetFloat Model_data.SvDT_MAX vars0} in
                  case process_events_helper events sim_log of {
                   (,) evs' sim1 ->
                    let {
                     sim1log = Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's'
                                 ((:) 's' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:)
                                 's' ((:) ':' ((:) 's' ((:) 'i' ((:) 'm' ((:) '1'
                                 ([])))))))))))))))))))) sim1}
                    in
                    let {vars' = Driver2.vars sim1log} in
                    let {min_time0 = Driver2.svGetFloat Model_data.SvT_STOP vars'} in
                    let {min_time = min_event_time evs' min_time0} in
                    let {time_to_next_event = Floats._Float__sub min_time t} in
                    let {
                     new_dt = case (Prelude.&&)
                                     (Floats._Float__cmp Integers.Cgt time_to_next_event
                                       Floats._Float__zero)
                                     (Floats._Float__cmp Integers.Cgt
                                       (Floats._Float__sub dt_max time_to_next_event)
                                       Driver2.epsilon) of {
                               Prelude.True -> time_to_next_event;
                               Prelude.False -> dt_max}}
                    in
                    let {sim3 = Driver2.set_var Model_data.SvDT new_dt sim1log} in
                    let {
                     sim4 = RecordSet.set Driver2.sim_events (\f e -> Driver2.Coq_mkSim
                              (Driver2.vars e) (Driver2.solkeys e) (Driver2.solution e)
                              (f (Driver2.sim_events e)) (Driver2.log e) (Driver2.flags e))
                              (RecordSet.constructor evs') sim3}
                    in
                    Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's' ((:) 's'
                      ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:) ':' ((:)
                      's' ((:) 'i' ((:) 'm' ((:) '4' ([])))))))))))))))))))) sim4}}}
  in
  Driver2.log_sim ((:) 'p' ((:) 'r' ((:) 'o' ((:) 'c' ((:) 'e' ((:) 's' ((:) 's' ((:) '_' ((:) 'e'
    ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:) ':' ((:) 'r' ((:) 'e' ((:) 's' ((:) 'u' ((:)
    'l' ((:) 't' ([])))))))))))))))))))))) result_sim

advance_states :: (([]) ((,) Model_data.Coq_stateVar Model_data.Coq_stateVar)) -> Floats.Coq_float ->
                  Driver2.Coq_simTy -> Driver2.Coq_simTy
advance_states pairs dt sim =
  let {
   result_sim = case pairs of {
                 ([]) -> sim;
                 (:) p pairstl ->
                  case p of {
                   (,) sv svd ->
                    let {vars0 = Driver2.vars sim} in
                    let {el = Driver2.svGetFloat sv vars0} in
                    let {eld = Driver2.svGetFloat svd vars0} in
                    let {el' = Floats._Float__add el (Floats._Float__mul eld dt)} in
                    let {sim1 = Driver2.set_var sv el' sim} in
                    let {
                     sim1log = Driver2.log_sim ((:) 'a' ((:) 'd' ((:) 'v' ((:) 'a' ((:) 'n' ((:) 'c'
                                 ((:) 'e' ((:) '_' ((:) 's' ((:) 't' ((:) 'a' ((:) 't' ((:) 'e' ((:)
                                 's' ((:) ':' ((:) 's' ((:) 'i' ((:) 'm' ((:) '1'
                                 ([])))))))))))))))))))) sim1}
                    in
                    advance_states pairstl dt sim1log}}}
  in
  Driver2.log_sim ((:) 'a' ((:) 'd' ((:) 'v' ((:) 'a' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_' ((:) 's'
    ((:) 't' ((:) 'a' ((:) 't' ((:) 'e' ((:) 's' ((:) ':' ((:) 'r' ((:) 'e' ((:) 's' ((:) 'u' ((:)
    'l' ((:) 't' ([])))))))))))))))))))))) result_sim

advance_model :: Driver2.Coq_simTy -> Driver2.Coq_simTy
advance_model sim =
  let {
   sim_log = Driver2.log_sim ((:) 'a' ((:) 'd' ((:) 'v' ((:) 'a' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_'
               ((:) 'm' ((:) 'o' ((:) 'd' ((:) 'e' ((:) 'l' ((:) ':' ((:) 's' ((:) 'i' ((:) 'm'
               ([])))))))))))))))))) sim}
  in
  let {vars0 = Driver2.vars sim_log} in
  let {dt = Driver2.svGetFloat Model_data.SvDT vars0} in
  let {
   pairs = (:) ((,) Model_data.SvX Model_data.SvXD) ((:) ((,) Model_data.SvXD Model_data.SvXDD)
    ([]))}
  in
  let {sim2 = advance_states pairs dt sim_log} in
  let {
   sim2log = Driver2.log_sim ((:) 'a' ((:) 'd' ((:) 'v' ((:) 'a' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_'
               ((:) 'm' ((:) 'o' ((:) 'd' ((:) 'e' ((:) 'l' ((:) ':' ((:) 's' ((:) 'i' ((:) 'm' ((:)
               '2' ([]))))))))))))))))))) sim2}
  in
  let {vars' = Driver2.vars sim2log} in
  let {t = Driver2.svGetFloat Model_data.SvT vars'} in
  let {new_t = Floats._Float__add t dt} in
  let {
   rounded_new_t = Floats._Float__div
                     (Float_text_io._FloatIO__round (Floats._Float__mul new_t tenTo6)) tenTo6}
  in
  let {result_sim = Driver2.set_var Model_data.SvT rounded_new_t sim2log} in
  Driver2.log_sim ((:) 'a' ((:) 'd' ((:) 'v' ((:) 'a' ((:) 'n' ((:) 'c' ((:) 'e' ((:) '_' ((:) 'm'
    ((:) 'o' ((:) 'd' ((:) 'e' ((:) 'l' ((:) ':' ((:) 'r' ((:) 'e' ((:) 's' ((:) 'u' ((:) 'l' ((:)
    't' ([]))))))))))))))))))))) result_sim

oneStep :: Driver2.Coq_simTy -> Driver2.Coq_simTy
oneStep sim =
  let {
   sim_log = Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p' ((:) ':'
               ((:) 's' ((:) 'i' ((:) 'm' ([])))))))))))) sim}
  in
  let {sim1 = Model_code.differential_equations sim_log} in
  let {
   sim1log = Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p' ((:) ':'
               ((:) 's' ((:) 'i' ((:) 'm' ((:) '1' ([]))))))))))))) sim1}
  in
  let {sim2 = process_events sim1log} in
  let {
   sim2log = Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p' ((:) ':'
               ((:) 's' ((:) 'i' ((:) 'm' ((:) '2' ([]))))))))))))) sim2}
  in
  let {
   sim3 = case Driver2.evaluate_xd (Driver2.flags sim2log) of {
           Prelude.True ->
            let {sim4 = Model_code.differential_equations sim2log} in
            let {
             sim4log = Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p'
                         ((:) ':' ((:) 's' ((:) 'i' ((:) 'm' ((:) '4' ([]))))))))))))) sim4}
            in
            let {flags4 = Driver2.flags sim4log} in
            let {
             flags4' = RecordSet.set Driver2.evaluate_xd (\f e -> Driver2.Coq_mkFlags
                         (Driver2.stop_simulation e) (Driver2.end_of_run e)
                         (f (Driver2.evaluate_xd e))) (RecordSet.constructor Prelude.False) flags4}
            in
            let {
             sim6 = RecordSet.set Driver2.flags (\f e -> Driver2.Coq_mkSim (Driver2.vars e)
                      (Driver2.solkeys e) (Driver2.solution e) (Driver2.sim_events e) (Driver2.log e)
                      (f (Driver2.flags e))) (RecordSet.constructor flags4') sim4log}
            in
            Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p' ((:) ':'
              ((:) 's' ((:) 'i' ((:) 'm' ((:) '6' ([]))))))))))))) sim6;
           Prelude.False -> sim2log}}
  in
  let {
   sim3log = Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p' ((:) ':'
               ((:) 's' ((:) 'i' ((:) 'm' ((:) '3' ([]))))))))))))) sim3}
  in
  let {sim5 = advance_model sim3log} in
  Driver2.log_sim ((:) 'o' ((:) 'n' ((:) 'e' ((:) 'S' ((:) 't' ((:) 'e' ((:) 'p' ((:) ':' ((:) 's'
    ((:) 'i' ((:) 'm' ((:) '5' ([]))))))))))))) sim5

run_sim_loop :: Prelude.Integer -> Driver2.Coq_simTy -> Driver2.Coq_simTy
run_sim_loop x x0 =
  Logic.and_rect (\_ _ steps sim ->
    let {
     hrec steps0 sim0 =
       case Coqlib.zle 0 steps0 of {
        Prelude.True ->
         case (Prelude.||) (Driver2.end_of_run (Driver2.flags sim0))
                (Driver2.stop_simulation (Driver2.flags sim0)) of {
          Prelude.True -> sim0;
          Prelude.False ->
           Specif.sig_rect (\rec_res _ -> rec_res)
             (hrec ((Prelude.-) steps0 ((\x -> x) 1)) (oneStep sim0))};
        Prelude.False -> sim0}}
    in hrec steps sim) x x0

run_sim :: Driver2.Coq_simTy -> Driver2.Coq_simTy
run_sim sim =
  let {
   sim_log = Driver2.log_sim ((:) 'r' ((:) 'u' ((:) 'n' ((:) '_' ((:) 's' ((:) 'i' ((:) 'm' ((:) ':'
               ((:) 's' ((:) 'i' ((:) 'm' ([])))))))))))) sim}
  in
  let {vars0 = Driver2.vars sim_log} in
  let {dtmin = Driver2.svGetFloat Model_data.SvDT_MIN vars0} in
  let {tstop = Driver2.svGetFloat Model_data.SvT_STOP vars0} in
  let {max_steps_float = Floats._Float__div tstop dtmin} in
  let {steps = Float_text_io._FloatIO__coq_ZofFloat max_steps_float} in
  let {result_sim = run_sim_loop steps sim_log} in
  Driver2.log_sim ((:) 'r' ((:) 'u' ((:) 'n' ((:) '_' ((:) 's' ((:) 'i' ((:) 'm' ((:) ':' ((:) 'r'
    ((:) 'e' ((:) 's' ((:) 'u' ((:) 'l' ((:) 't' ([]))))))))))))))) result_sim

sim_in :: () -> Driver2.Coq_simTy
sim_in _ =
  let {
   sim1log = Driver2.log_sim ((:) 's' ((:) 'i' ((:) 'm' ((:) '_' ((:) 'i' ((:) 'n' ((:) ':' ((:) 's'
               ((:) 'i' ((:) 'm' ((:) '1' ([])))))))))))) Driver2.default_sim_log}
  in
  let {sim2 = Model_code.init_sim sim1log} in
  Driver2.log_sim ((:) 's' ((:) 'i' ((:) 'm' ((:) '_' ((:) 'i' ((:) 'n' ((:) ':' ((:) 's' ((:) 'i'
    ((:) 'm' ((:) '2' ([])))))))))))) sim2

main :: () -> Driver2.Coq_simTy
main _ =
  let {sim1 = sim_in ()} in
  let {sim2 = run_sim sim1} in
  let {
   sim2log = Driver2.log_sim ((:) 's' ((:) 'i' ((:) 'm' ((:) '_' ((:) 'm' ((:) 'a' ((:) 'i' ((:) 'n'
               ((:) ':' ((:) 'r' ((:) 'e' ((:) 's' ((:) 'u' ((:) 'l' ((:) 't' ([]))))))))))))))))
               sim2}
  in
  let {
   sim3 = RecordSet.set Driver2.solution (\f e -> Driver2.Coq_mkSim (Driver2.vars e)
            (Driver2.solkeys e) (f (Driver2.solution e)) (Driver2.sim_events e) (Driver2.log e)
            (Driver2.flags e)) List0.rev sim2log}
  in
  RecordSet.set Driver2.log (\f e -> Driver2.Coq_mkSim (Driver2.vars e) (Driver2.solkeys e)
    (Driver2.solution e) (Driver2.sim_events e) (f (Driver2.log e)) (Driver2.flags e)) List0.rev sim3

