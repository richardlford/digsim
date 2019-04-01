Require Export Task.model_data.
Import FloatIO.
Import DebugIO.
Import DScopeNotations.
Import ListNotations.
Import RecordSetNotations'.
Open Scope D_scope.

From compcert Require Import Decidableplus.

Instance Decidable_eq_sv : forall (x y: stateVar), Decidable (eq x y) := Decidable_eq state_var_eq.

(*+ Map from the positive id for a stateVar to the stateVar. *)
Definition posToStateVarTreeType := PTree.t stateVar.
Definition emptyPosToStateVarTree := PTree.empty stateVar.

Fixpoint updatePosToStateVarTree (svIdStrList: list (stateVar * string))
         (tree: posToStateVarTreeType) : posToStateVarTreeType :=
  match svIdStrList with
  | nil => tree
  | cons (sv, str) xtail =>
    let tree1 := PTree.set (svIndex sv) sv tree in
    updatePosToStateVarTree xtail tree1
  end.

Definition posToStateVarTree :=
  updatePosToStateVarTree svStrList emptyPosToStateVarTree.

(* Map a positive to the corresonding stateVar *)
Definition posToStateVar (p: positive) := PTree.get p posToStateVarTree.

(*+ Maps with stateVar as domain *)
Module SvIndex <: Maps.INDEXED_TYPE.
  Definition t := stateVar.
  Definition index := svIndex.
  Definition eq := state_var_eq.
  Lemma index_inj:
    forall r1 r2, index r1 = index r2 -> r1 = r2.
  Proof.
    intros.
    destruct r1 eqn: r1eq; destruct r2 eqn: r2eq; simpl in *;try reflexivity;
      unfold index in H; simpl in H; inversion H.
  Qed.
End SvIndex.

Module SvTree := Maps.ITree(SvIndex).

(*+ Map from stateVar to stateVar name as string *)
Definition stringSvTreeTy := SvTree.t string.
Definition emptyStringPTree := SvTree.empty string.

Fixpoint updateStringSvTree (valList: list (stateVar * string)) (intree: stringSvTreeTy) :=
  match valList with
  | nil => intree
  | cons (sv, val) vltail =>
    let tree1 := SvTree.set sv val intree in
    updateStringSvTree vltail tree1
  end.

Definition svToStringTree :=
  updateStringSvTree svStrList emptyStringPTree.

Definition svToStrOpt (sv: stateVar) : option string :=
  SvTree.get sv svToStringTree.

Lemma svToStrComplete:
  forall sv : stateVar, exists s : string,
      svToStrOpt sv = Some s.
Proof.
  intros.
  destruct sv; unfold svToStrOpt;compute; eexists; f_equal.
Qed.

(* Since we have proven that svToStrOpt will always give a 
   result, define a function that extracts that value,
   and in the impossible case returns the empty string.
*)
Definition svToStr (sv: stateVar) : string :=
  match svToStrOpt sv with
  | Some x => x
  | None => ""
  end.

(*+ Map from stateVar to float, for simulation variables *)
Definition floatSvTreeTy := SvTree.t float.
Definition emptyFloatPTree := SvTree.empty float.

Fixpoint updateFloatSvTree (valList: list (stateVar * float)) (intree: floatSvTreeTy) :=
  match valList with
  | nil => intree
  | cons (sv, fval) vltail =>
    let tree1 := SvTree.set sv fval intree in
    updateFloatSvTree vltail tree1
  end.

Fixpoint printFloatKeyVals (kvl: list (stateVar * float)) :=
  match kvl with
  | nil => nil
  | cons (sv, fval) tl =>
    (sv, print_float fval) :: printFloatKeyVals tl
  end.


(*+ Defaults *)
Definition driver_defaults (_ : unit) :=
  [
            (SvT,        "0.0"#D);
            (SvT_STOP,   "0.0"#D);
            (SvDT,       "0.005"#D);
            (SvDT_MAX,   "0.005"#D);
            (SvDT_MIN,   "0.005"#D);
            (SvDT_PRINT, "0.01"#D)
  ].

(* Variable state after applying driver defaults and then model defaults
   that are not specific to auto subsystem (if not modularized). *)
Definition state0 (_ : unit) := (* Eval compute in *)
      let driverState := updateFloatSvTree (driver_defaults tt) emptyFloatPTree in
      updateFloatSvTree (model_default_values tt) driverState.

(* This takes a long time to prove, so during development comment it out. 

Lemma state0Complete:
  forall sv: stateVar, exists f: float,
      SvTree.get sv (state0 tt) = Some f.
Proof.
  intros.
  destruct sv;compute; eexists; f_equal.
Qed.

 *)

(* Since we have proven that state is complete, we can define a
   function that always returns a float, using a default
   in the impossible case.
 *)
Definition svGetFloat (sv: stateVar) (tree : floatSvTreeTy) :=
  match SvTree.get sv tree with
  | Some x => x
  | None => 0%D
  end.

(* Compute PTree.elements (state0 ()). *)


(* Give stateVar given its positive id, or if not a valid id,
   an arbitrary stateVar
*)
Definition posToStateVar' (pos: positive) :=
  match posToStateVar pos with
  | Some x => x
  | None => SvT (* Assume we always have time as a state variable. *)
  end.

Fixpoint printFloatStateElements (elist: list (positive * float)) : list (string * string) :=
  match elist with
  | nil => nil
  | cons (pos, fval) tl =>
    (svToStr (posToStateVar' pos), print_float fval) :: printFloatStateElements tl
  end.

Definition printFloatState (state: floatSvTreeTy) :=
  printFloatStateElements (PTree.elements state).

(* Compute printFloatState (state0 ()). *)

(*+ Simulation data structures *)
Record flagsTy :=
  mkFlags
    {
      stop_simulation : bool; (* Stop the simulation, t > tStop, *)
      end_of_run : bool;      (* Terminate early, user event logic. *)
      evaluate_xd : bool     (* Flag to determine of xd needs to reevaluated. *)
    }.

(* These Settable instances allow us to do partial update of records easily *)
Instance etaFlags  : Settable _ := 
  mkSettable (constructor mkFlags
                          <*> stop_simulation
                          <*> end_of_run
                          <*> evaluate_xd
             )%set.

Record eventTy :=
  mkEvent
    {
      key : string;
      time : float;
    }.

Instance etaEvent : Settable eventTy := 
  mkSettable (constructor mkEvent
                          <*> key
                          <*> time
             )%set.

Record logEntryTy :=
  mkLogEntry
    {
      le_caption: string;
      le_vars: list (string * string);
      le_events: list (string * string);
    }.

Instance etaLogEntry : Settable _ :=
  mkSettable (constructor mkLogEntry
                          <*> le_caption
                          <*> le_vars
                          <*> le_events
             )%set.

Record simTy :=
  mkSim
    {
      vars : floatSvTreeTy;              (* Simulation variables and constants. *)
      solkeys : list stateVar;           (* Keys of fields in Solution. *)
      solution : list (list string);     (* Simulation detail results. *)
      sim_events : list eventTy;         (* User events. *)
      log : list logEntryTy;             (* Text logging for debugging or information. *)
      flags : flagsTy                    (* control flags *)
    }.

Instance etaSim  : Settable _ := 
  mkSettable (constructor mkSim
                          <*> vars
                          <*> solkeys
                          <*> solution
                          <*> sim_events
                          <*> log
                          <*> flags
             )%set.

(*+ Utilitys for simulation structures *)
Definition printFloatSvTreeTy (vars: floatSvTreeTy) :=
  printFloatStateElements (PTree.elements vars).

(* Convert list of events to readable form *)
Fixpoint printEvents (evs: list eventTy) : list (string * string) :=
  match evs with
  | nil => nil
  | cons ev evtl =>
    let caption := ev.(key) in
    let time_str := print_float ev.(time) in
    (caption, time_str) :: printEvents evtl
  end.

(* Use this version if you want a log 
Definition log_sim (caption: string) (sim: simTy) : simTy :=
  let le := {|
        le_caption := caption;
        le_vars := printFloatSvTreeTy sim.(vars);
        le_events := printEvents sim.(sim_events)
      |} in
  let result_sim := sim[[log ::= (fun oldlog => le :: oldlog)]] in
  result_sim.

 *)

(* This is the dummy version *)
Definition log_sim (caption: string) (sim: simTy) : simTy := sim.

Definition default_flags :=
  {|
    stop_simulation := false;
    end_of_run := false;
    evaluate_xd := false;
  |}.

Definition set_vars (sim: simTy) (new_vars : floatSvTreeTy) :=
  let sim' := log_sim "set_vars:sim" sim in
  let new_vars' := printFloatSvTreeTy new_vars in
  let result_sim := sim[[vars := new_vars]] in
  let result_sim' := log_sim "set_vars: result" result_sim in
  result_sim'.

Definition set_var (key: stateVar) (val: float) (sim: simTy) :=
  let sim_log := log_sim "set_var:sim" sim in
  let result_sim := sim_log[[vars := SvTree.set key val sim.(vars)]] in
  let result_log := log_sim "set_var:result" result_sim in
  result_log.

Fixpoint union_vars (sim: simTy) (updates: list (stateVar * float)) :=
  match updates with
  | nil => sim
  | cons (sv, fval) remaining =>
    let sim1 := set_var sv fval sim in
    union_vars sim1 remaining
  end.

(*+ Common event functions and utilities *)
(*
  An Event function takes the event and the simulation state as inputs and
  returns a pair consisting of the updated simulation state and an
  optional float which, if present, is the time at which the event should
  be rescheduled. If the optional float is not present, then then
  event is not rescheduled, which, with the current event processing 
  algorithm, means that its time is unchanged, and it will continue
  to fire, since the condition for firing is that
  eventtime < currenttime + dtmin. In other words, the default for
  an event is for it to persist and it is only inhibited by scheduling
  it to some future time.
*)
Definition event_function_signature := eventTy -> simTy -> (simTy * option float).

Definition t_ge_tstop_event_func : event_function_signature :=
  fun (this: eventTy) (sim: simTy) =>
  let sim1 := log_sim "t_ge_tstop_event_func: sim1" sim in
  let vars := sim1.(vars) in
  let t := svGetFloat SvT vars in
  let t_stop := svGetFloat SvT_STOP vars in
  let stop_sim := t >=? t_stop in
  let new_flags := sim1.(flags)[[stop_simulation := stop_sim]] in
  let result_sim := sim1[[flags := new_flags]] in
  let result_log := log_sim "t_ge_tstop_event_func: result_sim" result_sim in
  (result_log, None).

Fixpoint make_solution_row_helper (vars: floatSvTreeTy) (keys: list stateVar) : list string :=
  match keys with
  | nil => nil
  | cons key others =>
    let fval := svGetFloat key vars in
    let sval := print_float fval in
    sval :: make_solution_row_helper vars others
  end.

Definition make_solution_row (sim: simTy) :=
  let row := make_solution_row_helper sim.(vars) sim.(solkeys) in
  let result_sim := sim[[solution ::= (fun old => row :: old)]] in
  let result_log := log_sim "make_solution_row: result" result_sim in
  result_log.

Definition append_solution_event_func  : event_function_signature :=
  fun (this: eventTy) (sim: simTy) =>
  let sim1 := make_solution_row sim in
  let sim1log := log_sim "append_solution_event_func:sim1" sim1 in
  let vars := sim1log.(vars) in
  let dt_print := svGetFloat SvDT_PRINT vars in
  let t := svGetFloat SvT vars in
  let new_t := t + dt_print in
  (sim1log, Some new_t).

Definition driver_default_events :=
  [
    {| key := "t_ge_tstop_event"; time := 0%D |};
    {| key := "append_log_event"; time := 0%D |}
  ].

Definition driver_default_handlers : list (string * event_function_signature) :=
  [
    ("t_ge_tstop_event", t_ge_tstop_event_func);
    ("append_log_event", append_solution_event_func)
  ].

Definition default_sim :=
  {|
    vars := (state0 tt);
    solkeys := modelOutputs;
    solution := nil;
    sim_events := driver_default_events;
    log := nil;
    flags := default_flags;
  |}.

Fixpoint handle_event (handlers: list (string * event_function_signature))
         (this: eventTy) (sim: simTy) :=
  match handlers with
  | nil => (sim, None)
  | cons (eventKey, evFun) otherHandlers =>
    if (eventKey =? this.(key))%string then
      evFun this sim
    else
      handle_event otherHandlers this sim
  end.

(* Update a list of events by changing the scheduled time for the 
   one with the specified function id.
*)
Fixpoint schedule_event (evs: list eventTy) (evkey: string) (new_time: float) :=
  match evs with
  | nil => nil
  | cons ev evtl =>
    let ev' :=
        if (ev.(key) =? evkey)%string then
          ev[[time := new_time]]
        else
          ev
    in
    ev' :: schedule_event evtl evkey new_time
  end.

(* pi matching Haskell Prelude Double *)
Definition pi := "3.141592653589793238"#D.

(* small floating point constant *)
Definition small := "0.000001#D".

(* comparison floating point constant *)
Definition epsilon := "0.0000000001"#D.

