From compcert Require Import Floats.
Require Import Coq.QArith.QArith.
Require Import Coq.Vectors.Vector.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Lists.List.
Require Import Orders.
Definition REAL_ARRAY_SIZE := 1000.
Definition MAX_NUMBER_OF_REAL_STATES := 5.
Definition Ndes := 2.

Open Scope string_scope.
Open Scope type_scope.

Definition newline : string := String "013"%char (String "010"%char "").

Definition event := string * list string.

Require Import Coq.Sorting.Mergesort.

Module EventOrder <: TotalLeBool.
  Definition t := (float * event).
  Definition leb (x:t) (y:t) := (match (Float.cmp Integers.Cge (fst x) (fst y)) with
                                |true => true
                                |false => match (Float.cmp Integers.Cge (fst y) (fst x)) with
                                         |true => false
                                         |false => true
                                         end
                                end).
  
  Theorem leb_total : forall a1 a2, is_true  (leb a1 a2) \/ is_true (leb a2 a1).
    unfold t.
    intros [t1 e1] [t2 e2].
    unfold leb.
    simpl.
    destruct (Float.cmp Integers.Cge t1 t2).
    cbv.
    constructor 1.
    trivial.
    cbv.
    constructor 2.
    destruct (Float.cmp Integers.Cge t2 t1).
    trivial.
    trivial.
    Defined.
End EventOrder.

Module Import EventSort := Sort EventOrder.

Record State : Type := mkState
                        {
                          x : t float Ndes;
                          xDot : t float Ndes;
                          
                          Time : float;
                          Time0 : float;
                          Tstop : float;
                          
                          DtMin : float;
                          DtMax : float;
                          
                          Damping_Coefficient : float;
                          Gravity : float;
                          Mass : float;
                          
                          Spring_Coefficient : float;
                          X_IC : float;
                          Xd_IC : float;

                          out : string;
                          events : list (float * event)
                        }.

Definition Set_x(st:State)(index:Fin.t Ndes) : float -> State :=
  fun f:float => mkState
                (replace (x st) index f) (xDot st)
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_xDot(st:State)(index:Fin.t Ndes) : float -> State :=
  fun f:float =>  mkState
                 (x st) (replace (xDot st) index f)
                 (Time st) (Time0 st) (Tstop st)
                 (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                 (Mass st) (Spring_Coefficient st) (X_IC st)
                 (Xd_IC st) (out st) (events st).

Fixpoint eventCleaner (time:float)(es:list (float * event)) : list (float * event) :=
  match es with
  |nil => nil
  |(t, e) :: tl => match (Float.cmp Integers.Cge t time) with
           |true => (t, e) :: (eventCleaner time tl)
           |false => (eventCleaner time tl)
           end
  end.

Definition cleanEvents(st:State) : State :=  mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (eventCleaner (Time st) (events st)).
 
Definition Set_Time(st:State) : float -> State :=
  fun f:float => cleanEvents (mkState
                             (x st) (xDot st) 
                             f (Time0 st) (Tstop st)
                             (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                             (Mass st) (Spring_Coefficient st) (X_IC st)
                             (Xd_IC st) (out st) (events st)).

Definition Set_Time0(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) f (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_Tstop(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st)
                (Time st) (Time0 st) f
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_DtMin(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                f (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_DtMax(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMax st) f (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_Damping_Coefficient(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) f (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_Gravity(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st)
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) f
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_Mass(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                f (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) (events st).

Definition Set_Spring_Coefficient(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) f (X_IC st)
                (Xd_IC st) (out st) (events st).


Definition Set_X_IC(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st)
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) f
                (Xd_IC st) (out st) (events st).

Definition Set_Xd_IC(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                f (out st) (events st).
Open Scope string_scope.
Definition print(st:State)(s:string) : State :=
  mkState
    (x st) (xDot st) 
    (Time st) (Time0 st) (Tstop st)
    (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
    (Mass st) (Spring_Coefficient st) (X_IC st)
    (Xd_IC st)
    (match (out st) with
     |"" => s
     |record => record ++ newline ++ s
     end) (events st).

Definition schedule(st:State)(t:float)(e:event) : State :=
  mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st)
  (match (Float.cmp Integers.Cge t (Time st)) with
   |true => sort ((t, e) :: (events st))
   |false => (events st)
   end).

Definition nextEvent(st:State) : option (State * (float * event)) :=
  match (events st) with
  | nil => None
  | (t, e) :: es => match (andb (Float.cmp Integers.Cge (Float.add (Time st) (DtMax st)) t) (Float.cmp Integers.Cge t (Time st))) with
                  |true => Some (
                              mkState
                                                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (DtMin st) (DtMax st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st) es,
                              (t, e))
                 |false => None
                 end
  end.