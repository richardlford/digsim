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
Definition width := 15%nat.
Definition fdigs := 6%nat.
Definition f2s := float_to_string width fdigs.
Definition fl2s := float_list_to_string width fdigs true.

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

(*+ Float scope notations *)
(* D for double float *)
Bind Scope D_scope with float.
Delimit Scope D_scope with D.

Infix "+" := Float.add : D_scope.
Notation "- x" := (Float.neg x) : D_scope.
Infix "-" := Float.sub : D_scope.
Infix "*" := Float.mul : D_scope.
Infix "/" := Float.div : D_scope.
Infix "?=" := Float.compare (at level 70, no associativity) : D_scope.

Infix "=?" := (Float.cmp Ceq) (at level 70, no associativity) : D_scope.
Infix "<=?" := (Float.cmp Cle) (at level 70, no associativity) : D_scope.
Infix "<?" := (Float.cmp Clt) (at level 70, no associativity) : D_scope.
Infix ">=?" := (Float.cmp Cge) (at level 70, no associativity) : D_scope.
Infix ">?" := (Float.cmp Cgt) (at level 70, no associativity) : D_scope.
Notation "0" := Float.zero : D_scope.

(* Variant of strToFloat when we know the string is valid *)
Definition strToFloat' (s: string) : float :=
  match strToFloat s with
  | Some x => x
  | None => 0%D
  end.

(* Inductive type used to name state variables. *)
Inductive StateVar : Set := 
| svT
| svX
| svXD
| svXDD
| svCOEFF_OF_REST
| svGRAVITY
| svT_STOP
| svDT
| svDT_MAX
| svDT_MIN
| svDT_PRINT.

Lemma state_var_eq: forall (r1 r2: StateVar), {r1 = r2} + {r1 <> r2}.
Proof. decide equality. Defined.
Global Opaque state_var_eq.

From compcert Require Import Decidableplus.

Instance Decidable_eq_sv : forall (x y: StateVar), Decidable (eq x y) := Decidable_eq state_var_eq.



(* For diagnostic purposes we want to map from the positive id for a
   StateVar to the StateVar. 
 *)
Definition PosToStateVarTree := PTree.t StateVar.
Definition EmptyPosToStateVarTree := PTree.empty StateVar.

Definition SvIndex (sv: StateVar) : positive :=
  match sv with
  | svT => 1
  | svX => 2
  | svXD => 3
  | svXDD => 4
  | svCOEFF_OF_REST => 5
  | svGRAVITY => 6
  | svT_STOP => 7
  | svDT => 8
  | svDT_MAX => 9
  | svDT_MIN => 10
  | svDT_PRINT => 11
  end.

Definition SvStrList :=
  [
    (svT, "T");
    (svX, "X");
    (svXD, "XD");
    (svXDD, "XDD");
    (svCOEFF_OF_REST, "COEFF_OF_REST");
    (svGRAVITY, "GRAVITY");
    (svT_STOP, "T_STOP");
    (svDT, "DT");
    (svDT_MAX, "DT_MAX");
    (svDT_MIN, "DT_MIN");
    (svDT_PRINT, "DT_PRINT")
  ]%positive.

Fixpoint UpdatePosToStateVarTree (svIdStrList: list (StateVar * string))
         (tree: PosToStateVarTree) : PosToStateVarTree :=
  match svIdStrList with
  | nil => tree
  | cons (sv, str) xtail =>
    let tree1 := PTree.set (SvIndex sv) sv tree in
    UpdatePosToStateVarTree xtail tree1
  end.

Definition posToStateVarTree :=
  Eval compute in UpdatePosToStateVarTree SvStrList EmptyPosToStateVarTree.

Definition posToStateVar (p: positive) := PTree.get p posToStateVarTree.

(*
Compute PTree.elements posToStateVarTree.
Compute posToStateVar 3.
*)

Module SvIndex <: Maps.INDEXED_TYPE.
  Definition t := StateVar.
  Definition index := SvIndex.
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

Definition StringSvTree := SvTree.t string.
Definition emptyStringPTree := SvTree.empty string.

Fixpoint UpdateStringSvTree (valList: list (StateVar * string)) (intree: StringSvTree) :=
  match valList with
  | nil => intree
  | cons (sv, val) vltail =>
    let tree1 := SvTree.set sv val intree in
    UpdateStringSvTree vltail tree1
  end.

Definition svToStringTree :=
  Eval compute in UpdateStringSvTree SvStrList emptyStringPTree.

Definition svToStrOpt (sv: StateVar) : option string :=
  SvTree.get sv svToStringTree.

Lemma svToStrComplete:
  forall sv : StateVar, exists s : string,
      svToStrOpt sv = Some s.
Proof.
  intros.
  destruct sv; unfold svToStrOpt;compute; eexists; f_equal.
Qed.

(* Since we have proven that svToStrOpt will always give a 
   result, define a function that extracts that value,
   and in the impossible case returns the empty string.
*)
Definition svToStr (sv: StateVar) : string :=
  match svToStrOpt sv with
  | Some x => x
  | None => ""
  end.

(*
Compute svToStr svDT.
Compute svToStr svDT_PRINT.
*)

Definition FloatSvTree := SvTree.t float.
Definition emptyFloatPTree := SvTree.empty float.

Fixpoint UpdateFloatSvTree (valList: list (StateVar * float)) (intree: FloatSvTree) :=
  match valList with
  | nil => intree
  | cons (sv, fval) vltail =>
    let tree1 := SvTree.set sv fval intree in
    UpdateFloatSvTree vltail tree1
  end.

(* Take state variable string pairs and convert to 
   state variable float pairs, by converting each
   string into a float. If the string cannot be converted,
   then skip it.
*)
Fixpoint StringKeyValToFloatKeyVal (kvl: list (StateVar * string)) : list (StateVar * float) :=
  match kvl with
  | nil => nil
  | cons (sv, str) tl =>
    let tailkvs := StringKeyValToFloatKeyVal tl in
    match strToFloat str with
    | Some fval => (sv, fval) :: tailkvs
    | None => tailkvs
    end
  end.

Fixpoint PrintFloatKeyVals (kvl: list (StateVar * float)) :=
  match kvl with
  | nil => nil
  | cons (sv, fval) tl =>
    (sv, f2s fval) :: PrintFloatKeyVals tl
  end.

Definition driver_defaults_str :=
  [
            (svT,        "0.0");
            (svT_STOP,   "0.0");
            (svDT,       "0.005");
            (svDT_MAX,   "0.005");
            (svDT_MIN,   "0.005");
            (svDT_PRINT, "0.01")
  ].

Definition driver_defaults :=
  Eval compute in StringKeyValToFloatKeyVal driver_defaults_str.

(* Compute PrintFloatKeyVals driver_defaults. *)

Definition model_default_values_str :=
  [
    (svT,             "0.0");
    (svX,             "10.0");
    (svXD,            "0.0");
    (svXDD,           "0.0");
    (svCOEFF_OF_REST, "0.80");
    (svGRAVITY,       "9.88");
    (svT_STOP,        "10.0");
    (svDT,            "0.01");
    (svDT_MAX,        "0.005");
    (svDT_MIN,        "0.001");
    (svDT_PRINT,      "0.01")
  ].

Definition model_default_values :=
  Eval compute in StringKeyValToFloatKeyVal model_default_values_str.

(* Compute PrintFloatKeyVals model_default_values. *)

Definition state0 := Eval compute in
      let driverState := UpdateFloatSvTree driver_defaults emptyFloatPTree in
      UpdateFloatSvTree model_default_values driverState.


Definition svToFloat0 (sv: StateVar) := SvTree.get sv state0.

Definition svToFloatStr0 (sv: StateVar) :=
  match svToFloat0 sv with
  | Some x => f2s x
  | None => "None"
  end.

(* Compute svToFloatStr0 svGRAVITY. *)

(* Takes a long time.
Lemma state0Complete:
  forall sv: StateVar, exists f: float,
      SvTree.get sv state0 = Some f.
Proof.
  intros.
  destruct sv;compute; eexists; f_equal.
Qed.
 *)

(* Since we have proven that state is complete, we can define a
   function that always returns a float, using a default
   in the impossible case.
 *)
Definition svGetFloat (sv: StateVar) (tree : FloatSvTree) :=
  match SvTree.get sv tree with
  | Some x => x
  | None => 0%D
  end.

(* Compute PTree.elements state0. *)


(* Give StateVar given its positive id, or if not a valid id,
   an arbitrary StateVar
*)
Definition posToStateVar' (pos: positive) :=
  match posToStateVar pos with
  | Some x => x
  | None => svT
  end.

Fixpoint PrintFloatStateElements (elist: list (positive * float)) : list (StateVar * string) :=
  match elist with
  | nil => nil
  | cons (pos, fval) tl =>
    (posToStateVar' pos, f2s fval) :: PrintFloatStateElements tl
  end.

Definition PrintFloatState (state: FloatSvTree) :=
  PrintFloatStateElements (PTree.elements state).

(* Compute PrintFloatState state0. *)

(*
Compute float_to_string 8 4 (svGetFloat svGRAVITY state0).
Compute SvTree.get svXD (SvTree.set svXD 0%D emptyFloatPTree).
*)

Definition modelOutputs : list StateVar := [svT; svX; svXD].
Print modelOutputs.

Record Flags :=
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

Inductive EventFuncId :=
| efiStop
| efiAppendSolution
| efiTerminateSim
| efiFlip_Xd_At_Bounce
.

Record Event :=
  mkEvent
    {
      key : string;
      time : float;
      func : EventFuncId
    }.

Instance etaEvent : Settable Event := 
  mkSettable (constructor mkEvent
                          <*> key
                          <*> time
                          <*> func
             )%set.

Record Sim :=
  mkSim
    {
      vars : FloatSvTree;              (* Simulation variables and constants. *)
      solkeys : list StateVar;         (* Keys of fields in Solution. *)
      solution : list (list string);   (* Simulation detail results. *)
      sim_events : list Event;   (* User events. *)
      log : list string;               (* Text logging for debugging or information. *)
      flags : Flags                    (* control flags *)
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

Definition t_ge_tstop_event_func (this: Event) (sim: Sim) : (Sim * option float) :=
  let vars := sim.(vars) in
  let t := svGetFloat svT vars in
  let t_stop := svGetFloat svT_STOP vars in
  let stop_sim := t >=? t_stop in
  let new_flags := sim.(flags)[[stop_simulation := stop_sim]] in
  (sim[[flags := new_flags]], None).

Fixpoint make_solution_row_helper (vars: FloatSvTree) (keys: list StateVar) : list string :=
  match keys with
  | nil => nil
  | cons key others =>
    let fval := svGetFloat key vars in
    let sval := f2s fval in
    sval :: make_solution_row_helper vars others
  end.

Definition make_solution_row (sim: Sim) :=
  let row := make_solution_row_helper sim.(vars) sim.(solkeys) in
  sim[[solution ::= (fun old => row :: old)]].

Definition append_solution_event_func (this: Event) (sim: Sim) : (Sim * option float) :=
  let sim1 := make_solution_row sim in
  let vars := sim1.(vars) in
  let dt_print := svGetFloat svDT_PRINT vars in
  let t := svGetFloat svT vars in
  let new_t := t + dt_print in
  (sim1, Some new_t).

Definition terminate_sim_event_func (this: Event) (sim: Sim) : (Sim * option float) :=
  (sim, None).

Definition driver_default_events :=
  [
    {| key := "t_ge_tstop_event"; time := 0%D; func := efiStop|};
    {| key := "append_log_event"; time := 0%D; func := efiAppendSolution|};
    {| key := "terminate_sim_event"; time := 0%D; func := efiTerminateSim|}
  ].

Definition default_sim :=
  {|
    vars := state0;
    solkeys := modelOutputs;
    solution := nil;
    sim_events := driver_default_events;
    log := nil;
    flags := default_flags;
  |}.

Definition F99 := Eval compute in strToFloat' "99.0".

Definition bounceEvent :=
  {| key := "flip_xd_at_bounce_event";
     time := F99;
     func := efiFlip_Xd_At_Bounce;
  |}.

Definition init_sim (sim: Sim) :=
  sim[[sim_events ::= (fun evs => bounceEvent :: evs)]].

Definition set_vars (sim: Sim) (vars' : FloatSvTree) :=
  sim[[vars := vars']].

Definition flip_xd_at_bounce_event_func (this: Event) (sim: Sim) : (Sim * option float) :=
  let vars := sim.(vars) in
  let coeff_of_rest := svGetFloat svCOEFF_OF_REST vars in
  let xd := svGetFloat svXD vars in
  let new_xd := - (coeff_of_rest * xd) in
  let vars' := SvTree.set svXD new_xd vars in
  let new_flags := sim.(flags)[[evaluate_xd := true]] in
  let sim' := set_vars sim vars' in
  let sim'' := sim'[[flags := new_flags]] in
  (sim', Some F99).

Definition handle_event (this: Event) (sim: Sim) : (Sim * option float) :=
  match this.(func) with
  | efiStop => t_ge_tstop_event_func this sim
  | efiAppendSolution => append_solution_event_func this sim
  | efiTerminateSim => terminate_sim_event_func this sim
  | efiFlip_Xd_At_Bounce => flip_xd_at_bounce_event_func this sim
  end.

Definition set_var (key: StateVar) (val: float) (sim: Sim) :=
  sim[[vars := SvTree.set key val sim.(vars)]].

Definition F0p5 := Eval compute in strToFloat' "0.5".
Definition Ftwo := Eval compute in strToFloat' "2.0".

(* pi matching Haskell Prelude Double *)
Definition PI := Eval compute in strToFloat' "3.141592653589793238".

(* small floating point constant *)
Definition SMALL := Eval compute in strToFloat' "0.000001".

(* comparison floating point constant *)
Definition EPSILON := Eval compute in strToFloat' "0.0000000001".
(* Compute float_to_string 0 19 PI. *)

Definition efi_eqb (fi1 fi2 : EventFuncId) : bool :=
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
Fixpoint schedule_event (evs: list Event) (efi: EventFuncId) (new_time: float) :=
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
Definition F9 := Eval compute in strToFloat' "9.0".
Definition sqrt9 := Eval compute in sqrt F9.
Compute f2s sqrt9.
 *)

Definition F1 := Eval compute in strToFloat' "1.0".

Definition differential_equations (sim: Sim) : Sim :=
  let vars := sim.(vars) in
  let t := svGetFloat svT vars in
  let x := svGetFloat svX vars in
  let xd := svGetFloat svXD vars in
  let dt_min := svGetFloat svDT_MIN vars in
  let dt_max := svGetFloat svDT_MAX vars in
  let gravity := svGetFloat svGRAVITY vars in
  let t_stop := svGetFloat svT_STOP vars in

  let xdd := -gravity in
  let sim1 := set_var svXDD xdd sim in
  let est_max := x + xd * dt_max + F0p5 * xdd * dt_max * dt_max in
  let dt_impact := t_stop + F1 in
  let dt_impact2 :=
      if (est_max <=? 0%D) then
        let est_min := x + xd * dt_min + F0p5 * xdd * dt_min * dt_min in
        if (est_min <=? 0%D) then
            0%D
        else
            let dt_impact3 := (-xd - sqrt(xd * xd - Ftwo * x * xdd)) / (Ftwo * x) in
            if (dt_min - dt_impact3 >? EPSILON) then
              (-xd + sqrt(xd * xd - Ftwo * x * xdd)) / (Ftwo * x)
            else
              dt_impact3
      else
        dt_impact in
  let impact_time := t + dt_impact2 in
  let events' := schedule_event sim1.(sim_events) efiFlip_Xd_At_Bounce impact_time in
  let sim2 := sim1[[sim_events := events']] in
  sim2.

Definition round (f: float) : Z :=
  let maybe_vlong := to_long f in
  let v64 :=
      match maybe_vlong with
      | Some x => x
      | None => Int64.zero
      end in
  Int64.intval v64.

Definition TenTo6 := Eval compute in strToFloat' "1.e6".

Definition process_one_event (ev: Event) (sim: Sim) :=
  let vars := sim.(vars) in
  let t := svGetFloat svT vars in
  let dt_min := svGetFloat svDT_MIN vars in
  let dt_max := svGetFloat svDT_MAX vars in
  let et := ev.(time) in
  let etdelta := round ((et - t) * TenTo6) in
  let minrnd := round (dt_min * TenTo6) in
  let (ev', sim') :=
      if (etdelta <? minrnd)%Z then
        let (sim2, new_time_opt) := handle_event ev sim in
        match new_time_opt with
        | Some new_time => (ev[[time := new_time]], sim2)
        | None => (ev, sim2)
        end
      else
        (ev, sim)
  in
  (ev', sim').

Fixpoint process_events_helper (evs: list Event) (sim: Sim) :=
  match evs with
  | nil => (nil, sim)
  | cons ev evtl =>
    let (ev', sim') := process_one_event ev sim in
    let (evs', sim'') := process_events_helper evtl sim' in
    (ev' :: evs', sim'')
  end.

Fixpoint min_event_time (evs: list Event) (min_so_far: float) : float :=
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

  
Definition process_events (sim: Sim) : Sim :=
  let vars0 := sim.(vars) in
  let events := sim.(sim_events) in
  match events with
  | nil => set_var svDT (svGetFloat svDT_MAX vars0) sim
  | cons x x0 =>
    let t := svGetFloat svT vars0 in
    let dt_max := svGetFloat svDT_MAX vars0 in
    let (evs', sim') := process_events_helper events sim in
    let vars' := sim'.(vars) in
    let min_time0 := svGetFloat svT_STOP vars' in
    let min_time := min_event_time evs' min_time0 in
    let time_to_next_event := min_time - t in
    let new_dt :=
        if (time_to_next_event >? 0%D) && ((dt_max - time_to_next_event) >? EPSILON) then
          time_to_next_event
        else
          dt_max
    in
    let sim3 := set_var svDT new_dt sim' in
    let sim4 := sim3[[sim_events := evs']] in
    sim4
  end.

Fixpoint advance_states (pairs: list (StateVar * StateVar)) (dt: float) (sim: Sim) : Sim :=
  match pairs with
  | nil => sim
  | cons (sv, svd) pairstl =>
    let vars0 := sim.(vars) in
    let el := svGetFloat sv vars0 in
    let eld := svGetFloat svd vars0 in
    let el' := el + eld * dt in
    let sim' := set_var sv el' sim in
    let sim'' := advance_states pairstl dt sim' in
    sim''
  end.

Definition advance_model (sim: Sim) : Sim :=
  let vars0 := sim.(vars) in
  let dt := svGetFloat svDT vars0 in
  let pairs := [(svX, svXD); (svXD, svXDD) ] in
  let sim' := advance_states pairs dt sim in
  let vars' := sim'.(vars) in
  let t := svGetFloat svT vars' in
  let new_t := (Z_to_float (round(t * TenTo6)))/TenTo6 in
  let sim'' := set_var svT new_t sim' in
  sim''.

Definition oneStep (sim: Sim) : Sim :=
  let sim1 := differential_equations sim in
  let sim2 := process_events sim1 in
  let sim3 :=
      if sim2.(flags).(evaluate_xd) then
        let sim4 := differential_equations sim2 in
        let flags4 := sim4.(flags) in
        let flags4' := flags4[[evaluate_xd := false]] in
        sim4[[flags := flags4']]
      else
        sim2 in
  let sim5 := advance_model sim3 in
  sim5.

Require Import Zwf.
From compcert Require Import Coqlib.

Function run_sim_loop (steps: Z) (sim: Sim) { wf (Zwf 0%Z) steps } :=
  if zle 0%Z steps
  then
    let f := sim.(flags) in
    if f.(end_of_run) || f.(stop_simulation)
    then sim
    else run_sim_loop (steps - 1) (oneStep sim)
  else sim.
Proof.
  intros; red; omega.
  apply Zwf_well_founded.
Qed.

(*
Definition binary64 := binary_float 53 1024.
*)

Definition ZofFloat (f: float) :=
  match Fappli_IEEE_extra.ZofB 53 1024 f with
  | Some z => z
  | None => 0
  end.

Definition run_sim (sim: Sim) :=
  let vars := sim.(vars) in
  let dtmin := svGetFloat svDT_MIN vars in
  let tstop := svGetFloat svT_STOP vars in
  let max_steps_float := (tstop / dtmin)%D in
  (*  let steps := ZofFloat max_steps_float in *)
  let steps := 10%Z in
  run_sim_loop steps sim.

Definition simin :=
  let sim1 := default_sim in
  let sim2 := init_sim sim1 in
  let sim3 := set_var svCOEFF_OF_REST (strToFloat' "0.88") sim2 in
  let sim4 := set_var svGRAVITY (strToFloat' "9.88") sim3 in
  sim4.

Definition simin' := Eval compute in simin.
Print simin'.

(* Events with interpreted floats *)
Record Event' :=
  mkEvent'
    {
      time' : string;
      func' : EventFuncId
    }.

(* Sim with interpreted floats *)
Record Sim' :=
  mkSim'
    {
      vars' : list (StateVar * string);
      solkeys' : list StateVar;         (* Keys of fields in Solution. *)
      solution' : list (list string);   (* Simulation detail results. *)
      sim_events' : list Event';        (* User events. *)
      log' : list string;                (* Text logging for debugging or information. *)
      flags' : Flags                     (* control flags *)
    }.

Definition PrintEvent (ev: Event) : Event' :=
  {| time' := f2s ev.(time); func' := ev.(func); |}.

Compute map PrintEvent simin'.(sim_events).

Definition PrintSim (sim: Sim) : Sim' :=
  {|
    vars' := map (fun kv : (positive * float) =>
                    let (key, fval) := kv in (posToStateVar' key, f2s fval)) (PTree.elements sim.(vars));
    solkeys' := sim.(solkeys);
    solution' := sim.(solution);
    sim_events' := map PrintEvent sim.(sim_events);
    log' := sim.(log);
    flags' := sim.(flags);
  |}.

Compute PrintSim simin'.

Definition difeq1 := differential_equations simin'.
Definition difeq1' := Eval compute in difeq1.
Compute PrintSim difeq1'.
Compute PrintSim simin'.

let sim5 := run_sim sim4 in
  sim5.

Definition result := Eval compute in main.
