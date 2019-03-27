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

val emptyPosToStateVarTree : stateVar PTree.t

val updatePosToStateVarTree : (stateVar * char list) list -> posToStateVarTreeType -> posToStateVarTreeType

val posToStateVarTree : posToStateVarTreeType

val posToStateVar : positive -> stateVar option

module SvIndex :
 sig
  type t = stateVar

  val index : stateVar -> positive

  val eq : stateVar -> stateVar -> bool
 end

module SvTree :
 sig
  type elt = SvIndex.t

  val elt_eq : SvIndex.t -> SvIndex.t -> bool

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
 end

type stringSvTreeTy = char list SvTree.t

val emptyStringPTree : char list SvTree.t

val updateStringSvTree : (stateVar * char list) list -> stringSvTreeTy -> stringSvTreeTy

val svToStringTree : stringSvTreeTy

val svToStrOpt : stateVar -> char list option

val svToStr : stateVar -> char list

type floatSvTreeTy = float SvTree.t

val emptyFloatPTree : float SvTree.t

val updateFloatSvTree : (stateVar * float) list -> floatSvTreeTy -> floatSvTreeTy

val stringKeyValToFloatKeyVal : (stateVar * char list) list -> (stateVar * float) list

val driver_defaults : unit -> (stateVar * float) list

val model_default_values : unit -> (stateVar * float) list

val state0 : unit -> floatSvTreeTy

val svGetFloat : stateVar -> floatSvTreeTy -> float

val posToStateVar' : positive -> stateVar

type flagsTy = { stop_simulation : bool; end_of_run : bool; evaluate_xd : bool }

val stop_simulation : flagsTy -> bool

val end_of_run : flagsTy -> bool

val evaluate_xd : flagsTy -> bool

type eventTy = { key : char list; time : float }

val key : eventTy -> char list

val time : eventTy -> float

type logEntryTy = { le_caption : char list; le_vars : (char list * char list) list;
                    le_events : (char list * char list) list }

type simTy = { vars : floatSvTreeTy; solkeys : stateVar list; solution : char list list list;
               sim_events : eventTy list; log : logEntryTy list; flags : flagsTy }

val vars : simTy -> floatSvTreeTy

val solkeys : simTy -> stateVar list

val solution : simTy -> char list list list

val sim_events : simTy -> eventTy list

val log : simTy -> logEntryTy list

val flags : simTy -> flagsTy

val log_sim : char list -> simTy -> simTy

val default_flags : flagsTy

val set_var : stateVar -> float -> simTy -> simTy

type event_function_signature = eventTy -> simTy -> simTy * float option

val t_ge_tstop_event_func : event_function_signature

val make_solution_row_helper : floatSvTreeTy -> stateVar list -> char list list

val make_solution_row : simTy -> simTy

val append_solution_event_func : event_function_signature

val terminate_sim_event_func : event_function_signature

val driver_default_events : eventTy list

val driver_default_handlers : (char list * event_function_signature) list

val default_sim : simTy

val default_sim_log : simTy

val handle_event : (char list * event_function_signature) list -> eventTy -> simTy -> simTy * float option

val epsilon : float
