open BinNums
open Floats
open Integers
open Maps
open RecordSet
open String0
open Debug_printers
open Float_text_io
open Model_data

type posToStateVarTreeType = stateVar PTree.t

(** val emptyPosToStateVarTree : stateVar PTree.t **)

let emptyPosToStateVarTree =
  PTree.empty

(** val updatePosToStateVarTree :
    (stateVar * char list) list -> posToStateVarTreeType ->
    posToStateVarTreeType **)

let rec updatePosToStateVarTree svIdStrList tree =
  match svIdStrList with
  | [] -> tree
  | p :: xtail ->
    let (sv, _) = p in
    let tree1 = PTree.set (svIndex sv) sv tree in
    updatePosToStateVarTree xtail tree1

(** val posToStateVarTree : posToStateVarTreeType **)

let posToStateVarTree =
  updatePosToStateVarTree svStrList emptyPosToStateVarTree

(** val posToStateVar : positive -> stateVar option **)

let posToStateVar p =
  PTree.get p posToStateVarTree

module SvIndex =
 struct
  type t = stateVar

  (** val index : stateVar -> positive **)

  let index =
    svIndex

  (** val eq : stateVar -> stateVar -> bool **)

  let eq =
    state_var_eq
 end

module SvTree = ITree(SvIndex)

type stringSvTreeTy = char list SvTree.t

(** val emptyStringPTree : char list SvTree.t **)

let emptyStringPTree =
  SvTree.empty

(** val updateStringSvTree :
    (stateVar * char list) list -> stringSvTreeTy -> stringSvTreeTy **)

let rec updateStringSvTree valList intree =
  match valList with
  | [] -> intree
  | p :: vltail ->
    let (sv, val0) = p in
    let tree1 = SvTree.set sv val0 intree in updateStringSvTree vltail tree1

(** val svToStringTree : stringSvTreeTy **)

let svToStringTree =
  updateStringSvTree svStrList emptyStringPTree

(** val svToStrOpt : stateVar -> char list option **)

let svToStrOpt sv =
  SvTree.get sv svToStringTree

(** val svToStr : stateVar -> char list **)

let svToStr sv =
  match svToStrOpt sv with
  | Some x -> x
  | None -> []

type floatSvTreeTy = float SvTree.t

(** val emptyFloatPTree : float SvTree.t **)

let emptyFloatPTree =
  SvTree.empty

(** val updateFloatSvTree :
    (stateVar * float) list -> floatSvTreeTy -> floatSvTreeTy **)

let rec updateFloatSvTree valList intree =
  match valList with
  | [] -> intree
  | p :: vltail ->
    let (sv, fval) = p in
    let tree1 = SvTree.set sv fval intree in updateFloatSvTree vltail tree1

(** val stringKeyValToFloatKeyVal :
    (stateVar * char list) list -> (stateVar * float) list **)

let rec stringKeyValToFloatKeyVal = function
| [] -> []
| p :: tl ->
  let (sv, str) = p in
  let tailkvs = stringKeyValToFloatKeyVal tl in
  (match FloatIO.strToFloat str with
   | Some fval -> (sv, fval) :: tailkvs
   | None -> tailkvs)

(** val driver_defaults : unit -> (stateVar * float) list **)

let driver_defaults _ =
  stringKeyValToFloatKeyVal driver_defaults_str

(** val model_default_values : unit -> (stateVar * float) list **)

let model_default_values _ =
  stringKeyValToFloatKeyVal model_default_values_str

(** val state0 : unit -> floatSvTreeTy **)

let state0 _ =
  let driverState = updateFloatSvTree (driver_defaults ()) emptyFloatPTree in
  updateFloatSvTree (model_default_values ()) driverState

(** val svGetFloat : stateVar -> floatSvTreeTy -> float **)

let svGetFloat sv tree =
  match SvTree.get sv tree with
  | Some x -> x
  | None -> Float.zero

(** val posToStateVar' : positive -> stateVar **)

let posToStateVar' pos =
  match posToStateVar pos with
  | Some x -> x
  | None -> SvT

type flagsTy = { stop_simulation : bool; end_of_run : bool; evaluate_xd : bool }

(** val stop_simulation : flagsTy -> bool **)

let stop_simulation x = x.stop_simulation

(** val end_of_run : flagsTy -> bool **)

let end_of_run x = x.end_of_run

(** val evaluate_xd : flagsTy -> bool **)

let evaluate_xd x = x.evaluate_xd

type eventTy = { key : char list; time : float }

(** val key : eventTy -> char list **)

let key x = x.key

(** val time : eventTy -> float **)

let time x = x.time

type logEntryTy = { le_caption : char list;
                    le_vars : (char list * char list) list;
                    le_events : (char list * char list) list }

type simTy = { vars : floatSvTreeTy; solkeys : stateVar list;
               solution : char list list list; sim_events : eventTy list;
               log : logEntryTy list; flags : flagsTy }

(** val vars : simTy -> floatSvTreeTy **)

let vars x = x.vars

(** val solkeys : simTy -> stateVar list **)

let solkeys x = x.solkeys

(** val solution : simTy -> char list list list **)

let solution x = x.solution

(** val sim_events : simTy -> eventTy list **)

let sim_events x = x.sim_events

(** val log : simTy -> logEntryTy list **)

let log x = x.log

(** val flags : simTy -> flagsTy **)

let flags x = x.flags

(** val log_sim : char list -> simTy -> simTy **)

let log_sim _ sim =
  sim

(** val default_flags : flagsTy **)

let default_flags =
  { stop_simulation = false; end_of_run = false; evaluate_xd = false }

(** val set_var : stateVar -> float -> simTy -> simTy **)

let set_var key0 val0 sim =
  let sim_log =
    log_sim
      ('s'::('e'::('t'::('_'::('v'::('a'::('r'::(':'::('s'::('i'::('m'::[])))))))))))
      sim
  in
  let result_sim =
    set vars (fun f e -> { vars = (f e.vars); solkeys = e.solkeys; solution =
      e.solution; sim_events = e.sim_events; log = e.log; flags = e.flags })
      (constructor (SvTree.set key0 val0 sim.vars)) sim_log
  in
  log_sim
    ('s'::('e'::('t'::('_'::('v'::('a'::('r'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))
    result_sim

(** val union_vars : simTy -> (stateVar * float) list -> simTy **)

let rec union_vars sim = function
| [] -> sim
| _ :: remaining -> union_vars sim remaining

type event_function_signature = eventTy -> simTy -> simTy * float option

(** val t_ge_tstop_event_func : event_function_signature **)

let t_ge_tstop_event_func _ sim =
  let sim1 =
    log_sim
      ('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))
      sim
  in
  let vars0 = sim1.vars in
  let t0 = svGetFloat SvT vars0 in
  let t_stop = svGetFloat SvT_STOP vars0 in
  let stop_sim = Float.cmp Cge t0 t_stop in
  let new_flags =
    set stop_simulation (fun f e -> { stop_simulation =
      (f e.stop_simulation); end_of_run = e.end_of_run; evaluate_xd =
      e.evaluate_xd }) (constructor stop_sim) sim1.flags
  in
  let result_sim =
    set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
      e.solution; sim_events = e.sim_events; log = e.log; flags =
      (f e.flags) }) (constructor new_flags) sim1
  in
  let result_log =
    log_sim
      ('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::('_'::('s'::('i'::('m'::[])))))))))))))))))))))))))))))))))
      result_sim
  in
  (result_log, None)

(** val make_solution_row_helper :
    floatSvTreeTy -> stateVar list -> char list list **)

let rec make_solution_row_helper vars0 = function
| [] -> []
| key0 :: others ->
  let fval = svGetFloat key0 vars0 in
  let sval = DebugIO.print_float fval in
  sval :: (make_solution_row_helper vars0 others)

(** val make_solution_row : simTy -> simTy **)

let make_solution_row sim =
  let row = make_solution_row_helper sim.vars sim.solkeys in
  let result_sim =
    set solution (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution =
      (f e.solution); sim_events = e.sim_events; log = e.log; flags =
      e.flags }) (fun old -> row :: old) sim
  in
  log_sim
    ('m'::('a'::('k'::('e'::('_'::('s'::('o'::('l'::('u'::('t'::('i'::('o'::('n'::('_'::('r'::('o'::('w'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))))))))
    result_sim

(** val append_solution_event_func : event_function_signature **)

let append_solution_event_func _ sim =
  let sim1 = make_solution_row sim in
  let sim1log =
    log_sim
      ('a'::('p'::('p'::('e'::('n'::('d'::('_'::('s'::('o'::('l'::('u'::('t'::('i'::('o'::('n'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))))))
      sim1
  in
  let vars0 = sim1log.vars in
  let dt_print = svGetFloat SvDT_PRINT vars0 in
  let t0 = svGetFloat SvT vars0 in
  let new_t = Float.add t0 dt_print in (sim1log, (Some new_t))

(** val terminate_sim_event_func : event_function_signature **)

let terminate_sim_event_func _ sim =
  let simlog =
    log_sim
      ('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('s'::('i'::('m'::[])))))))))))))))))))))))))))))
      sim
  in
  (simlog, None)

(** val driver_default_events : eventTy list **)

let driver_default_events =
  { key =
    ('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))));
    time = Float.zero } :: ({ key =
    ('a'::('p'::('p'::('e'::('n'::('d'::('_'::('l'::('o'::('g'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))));
    time = Float.zero } :: ({ key =
    ('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))));
    time = Float.zero } :: []))

(** val driver_default_handlers :
    (char list * event_function_signature) list **)

let driver_default_handlers =
  (('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))),
    t_ge_tstop_event_func) :: ((('a'::('p'::('p'::('e'::('n'::('d'::('_'::('l'::('o'::('g'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))),
    append_solution_event_func) :: ((('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))))))),
    terminate_sim_event_func) :: []))

(** val default_sim : simTy **)

let default_sim =
  { vars = (state0 ()); solkeys = modelOutputs; solution = []; sim_events =
    driver_default_events; log = []; flags = default_flags }

(** val default_sim_log : simTy **)

let default_sim_log =
  log_sim
    ('d'::('e'::('f'::('a'::('u'::('l'::('t'::('_'::('s'::('i'::('m'::[])))))))))))
    default_sim

(** val handle_event :
    (char list * event_function_signature) list -> eventTy -> simTy ->
    simTy * float option **)

let rec handle_event handlers this sim =
  match handlers with
  | [] -> (sim, None)
  | p :: otherHandlers ->
    let (eventKey, evFun) = p in
    if eqb eventKey this.key
    then evFun this sim
    else handle_event otherHandlers this sim

(** val epsilon : float **)

let epsilon =
  FloatIO.strToFloat'
    ('0'::('.'::('0'::('0'::('0'::('0'::('0'::('0'::('0'::('0'::('0'::('1'::[]))))))))))))
