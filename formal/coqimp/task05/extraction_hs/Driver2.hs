module Driver2 where

import qualified Prelude
import qualified Floats
import qualified Integers
import qualified Maps
import qualified RecordSet
import qualified Debug_printers
import qualified Float_text_io
import qualified Model_data

type Coq_posToStateVarTreeType = Maps.PTree__Coq_t Model_data.Coq_stateVar

emptyPosToStateVarTree :: Maps.PTree__Coq_t Model_data.Coq_stateVar
emptyPosToStateVarTree =
  Maps._PTree__empty

updatePosToStateVarTree :: (([]) ((,) Model_data.Coq_stateVar Prelude.String)) ->
                           Coq_posToStateVarTreeType -> Coq_posToStateVarTreeType
updatePosToStateVarTree svIdStrList tree =
  case svIdStrList of {
   ([]) -> tree;
   (:) p xtail ->
    case p of {
     (,) sv _ ->
      let {tree1 = Maps._PTree__set (Model_data.svIndex sv) sv tree} in
      updatePosToStateVarTree xtail tree1}}

posToStateVarTree :: Coq_posToStateVarTreeType
posToStateVarTree =
  updatePosToStateVarTree Model_data.svStrList emptyPosToStateVarTree

posToStateVar :: Prelude.Integer -> Prelude.Maybe Model_data.Coq_stateVar
posToStateVar p =
  Maps._PTree__get p posToStateVarTree

type SvIndex__Coq_t = Model_data.Coq_stateVar

_SvIndex__index :: Model_data.Coq_stateVar -> Prelude.Integer
_SvIndex__index =
  Model_data.svIndex

_SvIndex__eq :: Model_data.Coq_stateVar -> Model_data.Coq_stateVar -> Prelude.Bool
_SvIndex__eq =
  Model_data.state_var_eq

type SvTree__Coq_elt = SvIndex__Coq_t

_SvTree__elt_eq :: SvIndex__Coq_t -> SvIndex__Coq_t -> Prelude.Bool
_SvTree__elt_eq =
  _SvIndex__eq

type SvTree__Coq_t x = Maps.PTree__Coq_t x

_SvTree__empty :: SvTree__Coq_t a1
_SvTree__empty =
  Maps._PTree__empty

_SvTree__get :: SvTree__Coq_elt -> (SvTree__Coq_t a1) -> Prelude.Maybe a1
_SvTree__get k m =
  Maps._PTree__get (_SvIndex__index k) m

_SvTree__set :: SvTree__Coq_elt -> a1 -> (SvTree__Coq_t a1) -> SvTree__Coq_t a1
_SvTree__set k v m =
  Maps._PTree__set (_SvIndex__index k) v m

_SvTree__remove :: SvTree__Coq_elt -> (SvTree__Coq_t a1) -> SvTree__Coq_t a1
_SvTree__remove k m =
  Maps._PTree__remove (_SvIndex__index k) m

_SvTree__beq :: (a1 -> a1 -> Prelude.Bool) -> (SvTree__Coq_t a1) -> (SvTree__Coq_t a1) ->
                Prelude.Bool
_SvTree__beq =
  Maps._PTree__beq

_SvTree__combine :: ((Prelude.Maybe a1) -> (Prelude.Maybe a2) -> Prelude.Maybe a3) -> (SvTree__Coq_t
                    a1) -> (SvTree__Coq_t a2) -> SvTree__Coq_t a3
_SvTree__combine =
  Maps._PTree__combine

type Coq_stringSvTreeTy = SvTree__Coq_t Prelude.String

emptyStringPTree :: SvTree__Coq_t Prelude.String
emptyStringPTree =
  _SvTree__empty

updateStringSvTree :: (([]) ((,) Model_data.Coq_stateVar Prelude.String)) -> Coq_stringSvTreeTy ->
                      Coq_stringSvTreeTy
updateStringSvTree valList intree =
  case valList of {
   ([]) -> intree;
   (:) p vltail ->
    case p of {
     (,) sv val -> let {tree1 = _SvTree__set sv val intree} in updateStringSvTree vltail tree1}}

svToStringTree :: Coq_stringSvTreeTy
svToStringTree =
  updateStringSvTree Model_data.svStrList emptyStringPTree

svToStrOpt :: Model_data.Coq_stateVar -> Prelude.Maybe Prelude.String
svToStrOpt sv =
  _SvTree__get sv svToStringTree

svToStr :: Model_data.Coq_stateVar -> Prelude.String
svToStr sv =
  case svToStrOpt sv of {
   Prelude.Just x -> x;
   Prelude.Nothing -> ([])}

type Coq_floatSvTreeTy = SvTree__Coq_t Floats.Coq_float

emptyFloatPTree :: SvTree__Coq_t Floats.Coq_float
emptyFloatPTree =
  _SvTree__empty

updateFloatSvTree :: (([]) ((,) Model_data.Coq_stateVar Floats.Coq_float)) -> Coq_floatSvTreeTy ->
                     Coq_floatSvTreeTy
updateFloatSvTree valList intree =
  case valList of {
   ([]) -> intree;
   (:) p vltail ->
    case p of {
     (,) sv fval -> let {tree1 = _SvTree__set sv fval intree} in updateFloatSvTree vltail tree1}}

stringKeyValToFloatKeyVal :: (([]) ((,) Model_data.Coq_stateVar Prelude.String)) -> ([])
                             ((,) Model_data.Coq_stateVar Floats.Coq_float)
stringKeyValToFloatKeyVal kvl =
  case kvl of {
   ([]) -> ([]);
   (:) p tl ->
    case p of {
     (,) sv str ->
      let {tailkvs = stringKeyValToFloatKeyVal tl} in
      case Float_text_io._FloatIO__strToFloat str of {
       Prelude.Just fval -> (:) ((,) sv fval) tailkvs;
       Prelude.Nothing -> tailkvs}}}

driver_defaults :: () -> ([]) ((,) Model_data.Coq_stateVar Floats.Coq_float)
driver_defaults _ =
  stringKeyValToFloatKeyVal Model_data.driver_defaults_str

model_default_values :: () -> ([]) ((,) Model_data.Coq_stateVar Floats.Coq_float)
model_default_values _ =
  stringKeyValToFloatKeyVal Model_data.model_default_values_str

state0 :: () -> Coq_floatSvTreeTy
state0 _ =
  let {driverState = updateFloatSvTree (driver_defaults ()) emptyFloatPTree} in
  updateFloatSvTree (model_default_values ()) driverState

svGetFloat :: Model_data.Coq_stateVar -> Coq_floatSvTreeTy -> Floats.Coq_float
svGetFloat sv tree =
  case _SvTree__get sv tree of {
   Prelude.Just x -> x;
   Prelude.Nothing -> Floats._Float__zero}

posToStateVar' :: Prelude.Integer -> Model_data.Coq_stateVar
posToStateVar' pos =
  case posToStateVar pos of {
   Prelude.Just x -> x;
   Prelude.Nothing -> Model_data.SvT}

data Coq_flagsTy =
   Coq_mkFlags Prelude.Bool Prelude.Bool Prelude.Bool

stop_simulation :: Coq_flagsTy -> Prelude.Bool
stop_simulation f =
  case f of {
   Coq_mkFlags stop_simulation0 _ _ -> stop_simulation0}

end_of_run :: Coq_flagsTy -> Prelude.Bool
end_of_run f =
  case f of {
   Coq_mkFlags _ end_of_run0 _ -> end_of_run0}

evaluate_xd :: Coq_flagsTy -> Prelude.Bool
evaluate_xd f =
  case f of {
   Coq_mkFlags _ _ evaluate_xd0 -> evaluate_xd0}

data Coq_eventTy =
   Coq_mkEvent Prelude.String Floats.Coq_float

key :: Coq_eventTy -> Prelude.String
key e =
  case e of {
   Coq_mkEvent key0 _ -> key0}

time :: Coq_eventTy -> Floats.Coq_float
time e =
  case e of {
   Coq_mkEvent _ time0 -> time0}

data Coq_logEntryTy =
   Coq_mkLogEntry Prelude.String (([]) ((,) Prelude.String Prelude.String)) (([])
                                                                            ((,) Prelude.String
                                                                            Prelude.String))

data Coq_simTy =
   Coq_mkSim Coq_floatSvTreeTy (([]) Model_data.Coq_stateVar) (([]) (([]) Prelude.String)) (([])
                                                                                           Coq_eventTy) 
 (([]) Coq_logEntryTy) Coq_flagsTy

vars :: Coq_simTy -> Coq_floatSvTreeTy
vars s =
  case s of {
   Coq_mkSim vars0 _ _ _ _ _ -> vars0}

solkeys :: Coq_simTy -> ([]) Model_data.Coq_stateVar
solkeys s =
  case s of {
   Coq_mkSim _ solkeys0 _ _ _ _ -> solkeys0}

solution :: Coq_simTy -> ([]) (([]) Prelude.String)
solution s =
  case s of {
   Coq_mkSim _ _ solution0 _ _ _ -> solution0}

sim_events :: Coq_simTy -> ([]) Coq_eventTy
sim_events s =
  case s of {
   Coq_mkSim _ _ _ sim_events0 _ _ -> sim_events0}

log :: Coq_simTy -> ([]) Coq_logEntryTy
log s =
  case s of {
   Coq_mkSim _ _ _ _ log0 _ -> log0}

flags :: Coq_simTy -> Coq_flagsTy
flags s =
  case s of {
   Coq_mkSim _ _ _ _ _ flags0 -> flags0}

log_sim :: Prelude.String -> Coq_simTy -> Coq_simTy
log_sim _ sim =
  sim

default_flags :: Coq_flagsTy
default_flags =
  Coq_mkFlags Prelude.False Prelude.False Prelude.False

set_vars :: Coq_simTy -> Coq_floatSvTreeTy -> Coq_simTy
set_vars sim new_vars =
  let {
   result_sim = RecordSet.set vars (\f e -> Coq_mkSim (f (vars e)) (solkeys e) (solution e)
                  (sim_events e) (log e) (flags e)) (RecordSet.constructor new_vars) sim}
  in
  log_sim ((:) 's' ((:) 'e' ((:) 't' ((:) '_' ((:) 'v' ((:) 'a' ((:) 'r' ((:) 's' ((:) ':' ((:) ' '
    ((:) 'r' ((:) 'e' ((:) 's' ((:) 'u' ((:) 'l' ((:) 't' ([]))))))))))))))))) result_sim

set_var :: Model_data.Coq_stateVar -> Floats.Coq_float -> Coq_simTy -> Coq_simTy
set_var key0 val sim =
  let {
   sim_log = log_sim ((:) 's' ((:) 'e' ((:) 't' ((:) '_' ((:) 'v' ((:) 'a' ((:) 'r' ((:) ':' ((:) 's'
               ((:) 'i' ((:) 'm' ([])))))))))))) sim}
  in
  let {
   result_sim = RecordSet.set vars (\f e -> Coq_mkSim (f (vars e)) (solkeys e) (solution e)
                  (sim_events e) (log e) (flags e))
                  (RecordSet.constructor (_SvTree__set key0 val (vars sim))) sim_log}
  in
  log_sim ((:) 's' ((:) 'e' ((:) 't' ((:) '_' ((:) 'v' ((:) 'a' ((:) 'r' ((:) ':' ((:) 'r' ((:) 'e'
    ((:) 's' ((:) 'u' ((:) 'l' ((:) 't' ([]))))))))))))))) result_sim

type Coq_event_function_signature =
  Coq_eventTy -> Coq_simTy -> (,) Coq_simTy (Prelude.Maybe Floats.Coq_float)

t_ge_tstop_event_func :: Coq_event_function_signature
t_ge_tstop_event_func _ sim =
  let {
   sim1 = log_sim ((:) 't' ((:) '_' ((:) 'g' ((:) 'e' ((:) '_' ((:) 't' ((:) 's' ((:) 't' ((:) 'o'
            ((:) 'p' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) '_' ((:) 'f' ((:) 'u'
            ((:) 'n' ((:) 'c' ((:) ':' ((:) ' ' ((:) 's' ((:) 'i' ((:) 'm' ((:) '1'
            ([])))))))))))))))))))))))))))) sim}
  in
  let {vars0 = vars sim1} in
  let {t = svGetFloat Model_data.SvT vars0} in
  let {t_stop = svGetFloat Model_data.SvT_STOP vars0} in
  let {stop_sim = Floats._Float__cmp Integers.Cge t t_stop} in
  let {
   new_flags = RecordSet.set stop_simulation (\f e -> Coq_mkFlags (f (stop_simulation e))
                 (end_of_run e) (evaluate_xd e)) (RecordSet.constructor stop_sim) (flags sim1)}
  in
  let {
   result_sim = RecordSet.set flags (\f e -> Coq_mkSim (vars e) (solkeys e) (solution e)
                  (sim_events e) (log e) (f (flags e))) (RecordSet.constructor new_flags) sim1}
  in
  let {
   result_log = log_sim ((:) 't' ((:) '_' ((:) 'g' ((:) 'e' ((:) '_' ((:) 't' ((:) 's' ((:) 't' ((:)
                  'o' ((:) 'p' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ((:) '_' ((:)
                  'f' ((:) 'u' ((:) 'n' ((:) 'c' ((:) ':' ((:) ' ' ((:) 'r' ((:) 'e' ((:) 's' ((:)
                  'u' ((:) 'l' ((:) 't' ((:) '_' ((:) 's' ((:) 'i' ((:) 'm'
                  ([])))))))))))))))))))))))))))))))))) result_sim}
  in
  (,) result_log Prelude.Nothing

make_solution_row_helper :: Coq_floatSvTreeTy -> (([]) Model_data.Coq_stateVar) -> ([])
                            Prelude.String
make_solution_row_helper vars0 keys =
  case keys of {
   ([]) -> ([]);
   (:) key0 others ->
    let {fval = svGetFloat key0 vars0} in
    let {sval = Debug_printers._DebugIO__print_float fval} in
    (:) sval (make_solution_row_helper vars0 others)}

make_solution_row :: Coq_simTy -> Coq_simTy
make_solution_row sim =
  let {row = make_solution_row_helper (vars sim) (solkeys sim)} in
  let {
   result_sim = RecordSet.set solution (\f e -> Coq_mkSim (vars e) (solkeys e) (f (solution e))
                  (sim_events e) (log e) (flags e)) (\old -> (:) row old) sim}
  in
  log_sim ((:) 'm' ((:) 'a' ((:) 'k' ((:) 'e' ((:) '_' ((:) 's' ((:) 'o' ((:) 'l' ((:) 'u' ((:) 't'
    ((:) 'i' ((:) 'o' ((:) 'n' ((:) '_' ((:) 'r' ((:) 'o' ((:) 'w' ((:) ':' ((:) ' ' ((:) 'r' ((:)
    'e' ((:) 's' ((:) 'u' ((:) 'l' ((:) 't' ([])))))))))))))))))))))))))) result_sim

append_solution_event_func :: Coq_event_function_signature
append_solution_event_func _ sim =
  let {sim1 = make_solution_row sim} in
  let {
   sim1log = log_sim ((:) 'a' ((:) 'p' ((:) 'p' ((:) 'e' ((:) 'n' ((:) 'd' ((:) '_' ((:) 's' ((:) 'o'
               ((:) 'l' ((:) 'u' ((:) 't' ((:) 'i' ((:) 'o' ((:) 'n' ((:) '_' ((:) 'e' ((:) 'v' ((:)
               'e' ((:) 'n' ((:) 't' ((:) '_' ((:) 'f' ((:) 'u' ((:) 'n' ((:) 'c' ((:) ':' ((:) 's'
               ((:) 'i' ((:) 'm' ((:) '1' ([])))))))))))))))))))))))))))))))) sim1}
  in
  let {vars0 = vars sim1log} in
  let {dt_print = svGetFloat Model_data.SvDT_PRINT vars0} in
  let {t = svGetFloat Model_data.SvT vars0} in
  let {new_t = Floats._Float__add t dt_print} in (,) sim1log (Prelude.Just new_t)

terminate_sim_event_func :: Coq_event_function_signature
terminate_sim_event_func _ sim =
  let {
   simlog = log_sim ((:) 't' ((:) 'e' ((:) 'r' ((:) 'm' ((:) 'i' ((:) 'n' ((:) 'a' ((:) 't' ((:) 'e'
              ((:) '_' ((:) 's' ((:) 'i' ((:) 'm' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:)
              't' ((:) '_' ((:) 'f' ((:) 'u' ((:) 'n' ((:) 'c' ((:) ':' ((:) ' ' ((:) 's' ((:) 'i'
              ((:) 'm' ([])))))))))))))))))))))))))))))) sim}
  in
  (,) simlog Prelude.Nothing

driver_default_events :: ([]) Coq_eventTy
driver_default_events =
  (:) (Coq_mkEvent ((:) 't' ((:) '_' ((:) 'g' ((:) 'e' ((:) '_' ((:) 't' ((:) 's' ((:) 't' ((:) 'o'
    ((:) 'p' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ([])))))))))))))))))
    Floats._Float__zero) ((:) (Coq_mkEvent ((:) 'a' ((:) 'p' ((:) 'p' ((:) 'e' ((:) 'n' ((:) 'd' ((:)
    '_' ((:) 'l' ((:) 'o' ((:) 'g' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't'
    ([]))))))))))))))))) Floats._Float__zero) ((:) (Coq_mkEvent ((:) 't' ((:) 'e' ((:) 'r' ((:) 'm'
    ((:) 'i' ((:) 'n' ((:) 'a' ((:) 't' ((:) 'e' ((:) '_' ((:) 's' ((:) 'i' ((:) 'm' ((:) '_' ((:)
    'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ([])))))))))))))))))))) Floats._Float__zero) ([])))

driver_default_handlers :: ([]) ((,) Prelude.String Coq_event_function_signature)
driver_default_handlers =
  (:) ((,) ((:) 't' ((:) '_' ((:) 'g' ((:) 'e' ((:) '_' ((:) 't' ((:) 's' ((:) 't' ((:) 'o' ((:) 'p'
    ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ([]))))))))))))))))) t_ge_tstop_event_func)
    ((:) ((,) ((:) 'a' ((:) 'p' ((:) 'p' ((:) 'e' ((:) 'n' ((:) 'd' ((:) '_' ((:) 'l' ((:) 'o' ((:)
    'g' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e' ((:) 'n' ((:) 't' ([])))))))))))))))))
    append_solution_event_func) ((:) ((,) ((:) 't' ((:) 'e' ((:) 'r' ((:) 'm' ((:) 'i' ((:) 'n' ((:)
    'a' ((:) 't' ((:) 'e' ((:) '_' ((:) 's' ((:) 'i' ((:) 'm' ((:) '_' ((:) 'e' ((:) 'v' ((:) 'e'
    ((:) 'n' ((:) 't' ([])))))))))))))))))))) terminate_sim_event_func) ([])))

default_sim :: Coq_simTy
default_sim =
  Coq_mkSim (state0 ()) Model_data.modelOutputs ([]) driver_default_events ([]) default_flags

default_sim_log :: Coq_simTy
default_sim_log =
  log_sim ((:) 'd' ((:) 'e' ((:) 'f' ((:) 'a' ((:) 'u' ((:) 'l' ((:) 't' ((:) '_' ((:) 's' ((:) 'i'
    ((:) 'm' ([])))))))))))) default_sim

handle_event :: (([]) ((,) Prelude.String Coq_event_function_signature)) -> Coq_eventTy -> Coq_simTy
                -> (,) Coq_simTy (Prelude.Maybe Floats.Coq_float)
handle_event handlers this sim =
  case handlers of {
   ([]) -> (,) sim Prelude.Nothing;
   (:) p otherHandlers ->
    case p of {
     (,) eventKey evFun ->
      case (Prelude.==) eventKey (key this) of {
       Prelude.True -> evFun this sim;
       Prelude.False -> handle_event otherHandlers this sim}}}

schedule_event :: (([]) Coq_eventTy) -> Prelude.String -> Floats.Coq_float -> ([]) Coq_eventTy
schedule_event evs evkey new_time =
  case evs of {
   ([]) -> ([]);
   (:) ev evtl ->
    let {
     ev' = case (Prelude.==) (key ev) evkey of {
            Prelude.True ->
             RecordSet.set time (\f e -> Coq_mkEvent (key e) (f (time e)))
               (RecordSet.constructor new_time) ev;
            Prelude.False -> ev}}
    in
    (:) ev' (schedule_event evtl evkey new_time)}

epsilon :: Floats.Coq_float
epsilon =
  Float_text_io._FloatIO__strToFloat' ((:) '0' ((:) '.' ((:) '0' ((:) '0' ((:) '0' ((:) '0' ((:) '0'
    ((:) '0' ((:) '0' ((:) '0' ((:) '0' ((:) '1' ([])))))))))))))

