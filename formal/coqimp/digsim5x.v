(** DigSim

   DigSim provides a Coq architecture required to simulate continuous systems
   described by sets of simultaneous first-order differential equations.

     xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;


 ** _CoqProject config file contents

   -R ~/opt/lib/compcert/coq compcert -R . Top

 *)

From RecordUpdate Require Import RecordUpdate.
Module RecordSetNotations'.
  Delimit Scope record_set with rs.
  Open Scope rs.
  (*
    Note that the "set" function, after the proj argument, takes a function from 
    the old value to the new value. "(constructor v)" will make such a function
    that will ignore the old value and return v.
  *)
  Notation "x [[ proj  :=  v ]]" := (set proj (constructor v) x)
                                    (at level 12, left associativity) : record_set.
  Notation "x [[ proj  ::=  f ]]" := (set proj f x)
                                     (at level 12, f at next level, left associativity) : record_set.
End RecordSetNotations'.
Import RecordSetNotations'.

Require Import DigUty.float_text_io.
Import FloatIO.
Require Import DigUty.debug_printers.
Import DebugIO.

Require Import DigUty.Monad.

From compcert Require Import Floats.
Import Float.
From compcert Require Import Fappli_IEEE.
From compcert Require Import Integers.
Require Import Coq.Lists.List.

Import ListNotations.

Require Import BinNums.
Require Import Coq.QArith.QArith_base.
Require Import Strings.String.
Require Import Strings.Ascii.
Require Import Coq.ZArith.Znat.
Require Import Recdef.
From compcert Require Import Maps.

(* Inductive type used to name state variables. *)
Inductive stateVar : Set := 
| SvT
| SvX
| SvXD
| SvXDD
| SvCOEFF_OF_REST
| SvGRAVITY
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT.

Lemma state_var_eq: forall (r1 r2: stateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

From compcert Require Import Decidableplus.

Instance Decidable_eq_sv : forall (x y: stateVar), Decidable (eq x y) := Decidable_eq state_var_eq.

(* For diagnostic purposes we want to map from the positive id for a
   stateVar to the stateVar. 
 *)
Definition posToStateVarTreeType := PTree.t stateVar.
Definition emptyPosToStateVarTree := PTree.empty stateVar.

Definition svIndex (sv: stateVar) : positive :=
  match sv with
  | SvT => 1
  | SvX => 2
  | SvXD => 3
  | SvXDD => 4
  | SvCOEFF_OF_REST => 5
  | SvGRAVITY => 6
  | SvT_STOP => 7
  | SvDT => 8
  | SvDT_MAX => 9
  | SvDT_MIN => 10
  | SvDT_PRINT => 11
  end.

Definition svStrList :=
  [
    (SvT, "T");
    (SvX, "X");
    (SvXD, "XD");
    (SvXDD, "XDD");
    (SvCOEFF_OF_REST, "COEFF_OF_REST");
    (SvGRAVITY, "GRAVITY");
    (SvT_STOP, "T_STOP");
    (SvDT, "DT");
    (SvDT_MAX, "DT_MAX");
    (SvDT_MIN, "DT_MIN");
    (SvDT_PRINT, "DT_PRINT")
  ]%positive.

Fixpoint updatePosToStateVarTree (svIdStrList: list (stateVar * string))
         (tree: posToStateVarTreeType) : posToStateVarTreeType :=
  match svIdStrList with
  | nil => tree
  | cons (sv, str) xtail =>
    let tree1 := PTree.set (svIndex sv) sv tree in
    updatePosToStateVarTree xtail tree1
  end.

Definition posToStateVarTree :=
  (* Eval compute in *) updatePosToStateVarTree svStrList emptyPosToStateVarTree.

Definition posToStateVar (p: positive) := PTree.get p posToStateVarTree.

(*
Compute PTree.elements posToStateVarTree.
Compute posToStateVar 3.
*)

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
  (* Eval compute in *) updateStringSvTree svStrList emptyStringPTree.

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

(*
Compute svToStr SvDT.
Compute svToStr SvDT_PRINT.
*)

Definition floatSvTreeTy := SvTree.t float.
Definition emptyFloatPTree := SvTree.empty float.

Fixpoint updateFloatSvTree (valList: list (stateVar * float)) (intree: floatSvTreeTy) :=
  match valList with
  | nil => intree
  | cons (sv, fval) vltail =>
    let tree1 := SvTree.set sv fval intree in
    updateFloatSvTree vltail tree1
  end.

(* Take state variable string pairs and convert to 
   state variable float pairs, by converting each
   string into a float. If the string cannot be converted,
   then skip it.
*)
Fixpoint stringKeyValToFloatKeyVal (kvl: list (stateVar * string)) : list (stateVar * float) :=
  match kvl with
  | nil => nil
  | cons (sv, str) tl =>
    let tailkvs := stringKeyValToFloatKeyVal tl in
    match strToFloat str with
    | Some fval => (sv, fval) :: tailkvs
    | None => tailkvs
    end
  end.

Fixpoint printFloatKeyVals (kvl: list (stateVar * float)) :=
  match kvl with
  | nil => nil
  | cons (sv, fval) tl =>
    (sv, print_float fval) :: printFloatKeyVals tl
  end.

Definition driver_defaults_str :=
  [
            (SvT,        "0.0");
            (SvT_STOP,   "0.0");
            (SvDT,       "0.005");
            (SvDT_MAX,   "0.005");
            (SvDT_MIN,   "0.005");
            (SvDT_PRINT, "0.01")
  ].

Definition driver_defaults (_ : unit) :=
  (* Eval compute in *) stringKeyValToFloatKeyVal driver_defaults_str.

(* Compute printFloatKeyVals driver_defaults. *)

Definition model_default_values_str :=
  [
    (SvT,             "0.0");
    (SvX,             "10.0");
    (SvXD,            "0.0");
    (SvXDD,           "0.0");
    (SvCOEFF_OF_REST, "0.80");
    (SvGRAVITY,       "9.88");
    (SvT_STOP,        "10.0");
    (SvDT,            "0.01");
    (SvDT_MAX,        "0.005");
    (SvDT_MIN,        "0.001");
    (SvDT_PRINT,      "0.01")
  ].

Definition model_default_values (_ : unit) :=
  (* Eval compute in *) stringKeyValToFloatKeyVal model_default_values_str.

(* Compute printFloatKeyVals model_default_values. *)

Definition state0 (_ : unit) := (* Eval compute in *)
      let driverState := updateFloatSvTree (driver_defaults tt) emptyFloatPTree in
      updateFloatSvTree (model_default_values tt) driverState.

Definition svToFloat0 (sv: stateVar) := SvTree.get sv (state0 tt).

Definition svToFloatStr0 (sv: stateVar) :=
  match svToFloat0 sv with
  | Some x => print_float x
  | None => "None"
  end.

(* Compute svToFloatStr0 SvGRAVITY. *)

(* Takes a long time.
Lemma state0Complete:
  forall sv: stateVar, exists f: float,
      SvTree.get sv (state0 ()) = Some f.
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
  | None => SvT
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

(*
Compute float_to_string 8 4 (svGetFloat SvGRAVITY (state0 ())).
Compute SvTree.get SvXD (SvTree.set SvXD 0%D emptyFloatPTree).
*)

Definition modelOutputs : list stateVar := [SvT; SvX; SvXD].
Print modelOutputs.

Record flagsTy :=
  mkFlags
    {
      stop_simulation : bool; (* Stop the simulation, t > tStop, *)
      end_of_run : bool;      (* Terminate early, user event logic. *)
      evaluate_xd : bool     (* Flag to determine of xd needs to reevaluated. *)
    }.

Instance etaFlags  : Settable _ := 
  mkSettable (constructor mkFlags
                          <*> stop_simulation
                          <*> end_of_run
                          <*> evaluate_xd
             )%set.

Inductive eventFuncId :=
| efiStop
| efiAppendSolution
| efiTerminateSim
| efiFlip_Xd_At_Bounce
.

Record eventTy :=
  mkEvent
    {
      key : string;
      time : float;
      func : eventFuncId
    }.

Instance etaEvent : Settable eventTy := 
  mkSettable (constructor mkEvent
                          <*> key
                          <*> time
                          <*> func
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
      solkeys : list stateVar;         (* Keys of fields in Solution. *)
      solution : list (list string);   (* Simulation detail results. *)
      sim_events : list eventTy;   (* User events. *)
      log : list logEntryTy;               (* Text logging for debugging or information. *)
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

Definition printFloatSvTreeTy (vars: floatSvTreeTy) :=
  printFloatStateElements (PTree.elements vars).

Fixpoint logEvents (evs: list eventTy) : list (string * string) :=
  match evs with
  | nil => nil
  | cons ev evtl =>
    let caption := ev.(key) in
    let time_str := print_float ev.(time) in
    (caption, time_str) :: logEvents evtl
  end.

(* Use this version if you want a log 
Definition log_sim (caption: string) (sim: simTy) : simTy :=
  let le := {|
        le_caption := caption;
        le_vars := printFloatSvTreeTy sim.(vars);
        le_events := logEvents sim.(sim_events)
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

(* Return value is a pair of the updated sim, and an optional float which,
   if present, is the new time at which the event should be scheduled.
   The caller must do this.
 *)
Open Scope D_scope.

Definition t_ge_tstop_event_func (this: eventTy) (sim: simTy) : (simTy * option float) :=
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

Definition append_solution_event_func (this: eventTy) (sim: simTy) : (simTy * option float) :=
  let sim1 := make_solution_row sim in
  let sim1log := log_sim "append_solution_event_func:sim1" sim1 in
  let vars := sim1log.(vars) in
  let dt_print := svGetFloat SvDT_PRINT vars in
  let t := svGetFloat SvT vars in
  let new_t := t + dt_print in
  (sim1log, Some new_t).

Definition terminate_sim_event_func (this: eventTy) (sim: simTy) : (simTy * option float) :=
  let simlog := log_sim "terminate_sim_event_func: sim" sim in
  (simlog, None).

Definition driver_default_events :=
  [
    {| key := "t_ge_tstop_event"; time := 0%D; func := efiStop|};
    {| key := "append_log_event"; time := 0%D; func := efiAppendSolution|};
    {| key := "terminate_sim_event"; time := 0%D; func := efiTerminateSim|}
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

Definition default_sim_log:= log_sim "default_sim" default_sim.

Definition f99 := (* Eval compute in *) strToFloat' "99.0".

Definition bounceEvent :=
  {| key := "flip_xd_at_bounce_event";
     time := f99;
     func := efiFlip_Xd_At_Bounce;
  |}.

Definition init_sim (sim: simTy) :=
  let sim' := log_sim "init_sim:sim" sim in
  let result_sim := sim[[sim_events ::= (fun evs => bounceEvent :: evs)]] in
  let result_sim' := log_sim "init_sim: result" result_sim in
  result_sim'.

Definition set_vars (sim: simTy) (new_vars : floatSvTreeTy) :=
  let sim' := log_sim "set_vars:sim" sim in
  let new_vars' := printFloatSvTreeTy new_vars in
  let result_sim := sim[[vars := new_vars]] in
  let result_sim' := log_sim "set_vars: result" result_sim in
  result_sim'.


Definition flip_xd_at_bounce_event_func (this: eventTy) (sim: simTy) : (simTy * option float) :=
  let sim' := log_sim "flip_xd_at_bounce_event_func: sim" sim in
  let vars := sim'.(vars) in
  let coeff_of_rest := svGetFloat SvCOEFF_OF_REST vars in
  let xd := svGetFloat SvXD vars in
  let new_xd := - (coeff_of_rest * xd) in
  let vars' := SvTree.set SvXD new_xd vars in
  let new_flags := sim'.(flags)[[evaluate_xd := true]] in
  let sim2 := set_vars sim' vars' in
  let result_sim := sim2[[flags := new_flags]] in
  let result_log := log_sim "flip_xd_at_bounce_event_func" result_sim in
  (result_log, Some f99).

Definition handle_event (this: eventTy) (sim: simTy) : (simTy * option float) :=
  let sim_log := log_sim "handle_event:sim" sim in
  let result := 
      match this.(func) with
      | efiStop => t_ge_tstop_event_func this sim_log
      | efiAppendSolution => append_solution_event_func this sim_log
      | efiTerminateSim => terminate_sim_event_func this sim_log
      | efiFlip_Xd_At_Bounce => flip_xd_at_bounce_event_func this sim_log
      end in
  let (result_sim, maybefloat) := result in
  let result_log := log_sim "handle_event:result" result_sim in
  (result_log, maybefloat).

Definition set_var (key: stateVar) (val: float) (sim: simTy) :=
  let sim_log := log_sim "set_var:sim" sim in
  let result_sim := sim_log[[vars := SvTree.set key val sim.(vars)]] in
  let result_log := log_sim "set_var:result" result_sim in
  result_log.

Definition fhalf := (* Eval compute in *) strToFloat' "0.5".
Definition ftwo := (* Eval compute in *) strToFloat' "2.0".

(* pi matching Haskell Prelude Double *)
Definition pi := (* Eval compute in *) strToFloat' "3.141592653589793238".

(* small floating point constant *)
Definition small := (* Eval compute in *) strToFloat' "0.000001".

(* comparison floating point constant *)
Definition epsilon := (* Eval compute in *) strToFloat' "0.0000000001".
(* Compute float_to_string 0 19 pi. *)

Definition efi_eqb (fi1 fi2 : eventFuncId) : bool :=
  match fi1, fi2 with
  | efiStop, efiStop => true
  | efiAppendSolution, efiAppendSolution => true
  | efiTerminateSim, efiTerminateSim => true
  | efiFlip_Xd_At_Bounce, efiFlip_Xd_At_Bounce => true
  | _, _ => false
  end.

(* Update a list of events by changing the scheduled time for the 
   one with the specified function id.
*)
Fixpoint schedule_event (evs: list eventTy) (efi: eventFuncId) (new_time: float) :=
  match evs with
  | nil => nil
  | cons ev evtl =>
    let ev' :=
        if efi_eqb ev.(func) efi then
          ev[[time := new_time]]
        else
          ev
    in
    ev' :: schedule_event evtl efi new_time
  end.

Definition sqrt (arg: float) : float :=
  Fappli_IEEE_bits.b64_sqrt mode_NE arg.
(*
Definition F9 := (* Eval compute in *) strToFloat' "9.0".
Definition sqrt9 := (* Eval compute in *) sqrt F9.
Compute print_float sqrt9.
 *)

Definition fone := (* Eval compute in *) strToFloat' "1.0".

Definition differential_equations (sim: simTy) : simTy :=
  let sim_log := log_sim "differential_equations" sim in
  let vars := sim_log.(vars) in
  let t := svGetFloat SvT vars in
  let x := svGetFloat SvX vars in
  let xd := svGetFloat SvXD vars in
  let dt_min := svGetFloat SvDT_MIN vars in
  let dt_max := svGetFloat SvDT_MAX vars in
  let gravity := svGetFloat SvGRAVITY vars in
  let t_stop := svGetFloat SvT_STOP vars in

  let xdd := -gravity in
  let sim1 := set_var SvXDD xdd sim_log in
  let sim1log := log_sim "differential_equations:sim1" sim1 in
  let est_max := x + xd * dt_max + fhalf * xdd * dt_max * dt_max in
  let dt_impact := t_stop + fone in
  let dt_impact2 :=
      if (est_max <=? 0%D) then
        let est_min := x + xd * dt_min + fhalf * xdd * dt_min * dt_min in
        if (est_min <=? 0%D) then
            0%D
        else
            let dt_impact3 := (-xd - sqrt(xd * xd - ftwo * x * xdd)) / (ftwo * x) in
            if (dt_min - dt_impact3 >? epsilon) then
              (-xd + sqrt(xd * xd - ftwo * x * xdd)) / (ftwo * x)
            else
              dt_impact3
      else
        dt_impact in
  let impact_time := t + dt_impact2 in
  let events' := schedule_event sim1log.(sim_events) efiFlip_Xd_At_Bounce impact_time in
  let result_sim := sim1log[[sim_events := events']] in
  let result_log := log_sim "differential_equations" result_sim in
  result_log.

Definition ZofFloat (f: float) :=
  match Fappli_IEEE_extra.ZofB 53 1024 f with
  | Some z => z
  | None => 0%Z
  end.

Definition round (f: float) : float :=
  let z := ZofFloat (f + fhalf) in
  Z_to_float z.

Definition tenTo6 := (* Eval compute in *) strToFloat' "1.e6".

Definition process_one_event (ev: eventTy) (sim: simTy) :=
  let sim_log := log_sim "process_one_event:sim" sim in
  let vars := sim_log.(vars) in
  let t := svGetFloat SvT vars in
  let dt_min := svGetFloat SvDT_MIN vars in
  let dt_max := svGetFloat SvDT_MAX vars in
  let et := ev.(time) in
  let etdelta := round ((et - t) * tenTo6) in
  let minrnd := round (dt_min * tenTo6) in
  let (ev', result_sim) :=
      if (etdelta <? minrnd) then
        let (sim2, new_time_opt) := handle_event ev sim_log in
        let sim2log := log_sim "process_one_event:sim2" sim2 in
        match new_time_opt with
        | Some new_time => (ev[[time := new_time]], sim2log)
        | None => (ev, sim2log)
        end
      else
        (ev, sim_log)
  in
  let result_log := log_sim "process_one_event" result_sim in
  (ev', result_log).

Fixpoint process_events_helper (evs: list eventTy) (sim: simTy) :=
  match evs with
  | nil => (nil, sim)
  | cons ev evtl =>
    let (ev', sim') := process_one_event ev sim in
    let (evs', sim'') := process_events_helper evtl sim' in
    (ev' :: evs', sim'')
  end.

Fixpoint min_event_time (evs: list eventTy) (min_so_far: float) : float :=
  match evs with
  | nil => min_so_far
  | cons ev evtl =>
    let et := ev.(time) in
    let new_min :=
        if (et >? 0%D) && (et <? min_so_far) then
          et
        else
          min_so_far
    in
    min_event_time evtl new_min
  end.

  
Definition process_events (sim: simTy) : simTy :=
  let sim_log := log_sim "process_events:sim" sim in
  let vars0 := sim_log.(vars) in
  let events := sim_log.(sim_events) in
  let result_sim := 
      match events with
      | nil => set_var SvDT (svGetFloat SvDT_MAX vars0) sim_log
      | cons x x0 =>
        let t := svGetFloat SvT vars0 in
        let dt_max := svGetFloat SvDT_MAX vars0 in
        let (evs', sim1) := process_events_helper events sim_log in
        let sim1log := log_sim "process_events:sim1" sim1 in
        let vars' := sim1log.(vars) in
        let min_time0 := svGetFloat SvT_STOP vars' in
        let min_time := min_event_time evs' min_time0 in
        let time_to_next_event := min_time - t in
        let new_dt :=
            if (time_to_next_event >? 0%D) && ((dt_max - time_to_next_event) >? epsilon) then
              time_to_next_event
            else 
              dt_max
        in
        let sim3 := set_var SvDT new_dt sim1log in
        let sim4 := sim3[[sim_events := evs']] in
        let sim4log := log_sim "process_events:sim4" sim4 in
        sim4log
      end in
  let result_log := log_sim "process_events:result" result_sim in
  result_log.

Fixpoint advance_states (pairs: list (stateVar * stateVar)) (dt: float) (sim: simTy) : simTy :=
  let sim_log := log_sim "advance_states:sim" sim in
  let result_sim := 
      match pairs with
      | nil => sim
      | cons (sv, svd) pairstl =>
        let vars0 := sim.(vars) in
        let el := svGetFloat sv vars0 in
        let eld := svGetFloat svd vars0 in
        let el' := el + eld * dt in
        let sim1 := set_var sv el' sim in
        let sim1log := log_sim "advance_states:sim1" sim1 in
        let sim2 := advance_states pairstl dt sim1log in
        sim2
      end in
  let result_log := log_sim "advance_states:result" result_sim in
  result_log.

Definition advance_model (sim: simTy) : simTy :=
  let sim_log := log_sim "advance_model:sim" sim in
  let vars0 := sim_log.(vars) in
  let dt := svGetFloat SvDT vars0 in
  let pairs := [(SvX, SvXD); (SvXD, SvXDD) ] in
  let sim2 := advance_states pairs dt sim_log in
  let sim2log := log_sim "advance_model:sim2" sim2 in
  let vars' := sim2log.(vars) in
  let t := svGetFloat SvT vars' in
  let new_t := t + dt in
  let rounded_new_t := round (new_t * tenTo6)/tenTo6 in
  (* let rounded_new_t := new_t in *)
  let result_sim := set_var SvT rounded_new_t sim2log in
  let result_log := log_sim "advance_model:result" result_sim in
  result_log.

Definition oneStep (sim: simTy) : simTy :=
  let sim_log := log_sim "oneStep:sim" sim in
  let sim1 := differential_equations sim_log in
  let sim1log := log_sim "oneStep:sim1" sim1 in
  let sim2 := process_events sim1log in
  let sim2log := log_sim "oneStep:sim2" sim2 in
  let sim3 :=
      if sim2log.(flags).(evaluate_xd) then
        let sim4 := differential_equations sim2log in
        let sim4log := log_sim "oneStep:sim4" sim4 in
        let flags4 := sim4log.(flags) in
        let flags4' := flags4[[evaluate_xd := false]] in
        let sim6 := sim4log[[flags := flags4']] in
        let sim6log := log_sim "oneStep:sim6" sim6 in
        sim6log
      else
        sim2log in
  let sim3log := log_sim "oneStep:sim3" sim3 in
  let sim5 := advance_model sim3log in
  let sim5log := log_sim "oneStep:sim5" sim5 in
  sim5log.

Require Import Zwf.
From compcert Require Import Coqlib.

Function run_sim_loop (steps: Z) (sim: simTy) { wf (Zwf 0%Z) steps } :=
  let result_sim := 
      if zle 0%Z steps
      then
        let f := sim.(flags) in
        if f.(end_of_run) || f.(stop_simulation)
        then sim
        else
          let sim2 := (oneStep sim) in
          (* let sim2log := log_sim "run_sim_loop:sim2" sim2 in *)
          let steps2 := (steps - 1) in 
          run_sim_loop steps2 sim2
      else sim
  in
  result_sim.
Proof.
  intros; red; omega.
  apply Zwf_well_founded.
Qed.

(*
Definition binary64 := binary_float 53 1024.
*)

Definition run_sim (sim: simTy) :=
  let sim_log := log_sim "run_sim:sim" sim in
  let vars := sim_log.(vars) in
  let dtmin := svGetFloat SvDT_MIN vars in
  let tstop := svGetFloat SvT_STOP vars in
  let max_steps_float := (tstop / dtmin)%D in
  let steps := ZofFloat max_steps_float in
  let result_sim := run_sim_loop steps sim_log in
  let result_log := log_sim "run_sim:result" result_sim in
  result_log.

Definition sim_in (_ : unit) :=
  let sim1 := default_sim_log in
  let sim1log := log_sim "sim_in:sim1" sim1 in
  let sim2 := init_sim sim1log in
  let sim2log := log_sim "sim_in:sim2" sim2 in
  let sim3 := set_var SvCOEFF_OF_REST (strToFloat' "0.80") sim2log in
  let sim3log := log_sim "sim_in:sim3" sim3 in
  let sim4 := set_var SvGRAVITY (strToFloat' "9.88") sim3log in
  let sim4log := log_sim "sim_in:sim4" sim4 in
  sim4log.

Definition main (_ : unit) :=
  let sim1 := (sim_in tt) in
  let sim1log := log_sim "sim_main:sim1" sim1 in
  let sim2 := run_sim  sim1 in
  let sim2log := log_sim "sim_main:result" sim2 in
  let sim3 := sim2log[[solution ::= (fun sol => rev sol)]] in
  let sim4 := sim3[[log ::= (fun lg => rev lg)]] in
  sim4.

Extraction Language OCaml.
(* Unset Extraction Optimize. *)
(* Unset Extraction AutoInline. *)

(* Standard lib *)
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

(* Coqlib *)
Extract Inlined Constant Coqlib.proj_sumbool => "(fun x -> x)".

(* Datatypes *)
Extract Inlined Constant Datatypes.fst => "fst".
Extract Inlined Constant Datatypes.snd => "snd".

(* Decidable *)

Extraction Inline DecidableClass.Decidable_witness DecidableClass.decide
   Decidableplus.Decidable_and Decidableplus.Decidable_or
   Decidableplus.Decidable_not Decidableplus.Decidable_implies.

(* Avoid name clashes *)
Extraction Blacklist List String Int.

(* Cutting the dependency to R. *)
Extract Inlined Constant Fcore_defs.F2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.FF2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.B2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.round_mode => "fun _ -> assert false".
Extract Inlined Constant Fcalc_bracket.inbetween_loc => "fun _ -> assert false".
(* Go! *)

Cd "sep_extraction".

Separate Extraction main print_Z svToStr posToStateVar'.

Cd "..".
