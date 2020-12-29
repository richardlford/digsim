From compcert Require Import Floats.
Require Import Coq.QArith.QArith.
Require Import Coq.Vectors.Vector.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.

Definition REAL_ARRAY_SIZE := 1000.
Definition MAX_NUMBER_OF_REAL_STATES := 5.
Definition Ndes := 2.

Open Scope string_scope.

Definition newline : string := String "013"%char (String "010"%char "").

Record State : Type := mkState
                        {
                          x : t float Ndes;
                          xDot : t float Ndes;
                          Time : float;
                          
                          Time0 : float;
                          Tstop : float;
                          Dt : float;
                          
                          Damping_Coefficient : float;
                          Gravity : float;
                          Mass : float;
                          
                          Spring_Coefficient : float;
                          X_IC : float;
                          Xd_IC : float;

                          out : string
                        }.

Definition Set_x(st:State)(index:Fin.t Ndes) : float -> State :=
  fun f:float => mkState
                (replace (x st) index f) (xDot st)
                (Time st) (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_xDot(st:State)(index:Fin.t Ndes) : float -> State :=
  fun f:float =>  mkState
                 (x st) (replace (xDot st) index f)
                 (Time st) (Time0 st) (Tstop st)
                 (Dt st) (Damping_Coefficient st) (Gravity st)
                 (Mass st) (Spring_Coefficient st) (X_IC st)
                 (Xd_IC st) (out st).

Definition Set_Time(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                f (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Time0(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) f (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Tstop(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st)
                (Time st) (Time0 st) f
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Dt(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                f (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Damping_Coefficient(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (Dt st) f (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Gravity(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st)
                (Time st) (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) f
                (Mass st) (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Mass(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                f (Spring_Coefficient st) (X_IC st)
                (Xd_IC st) (out st).

Definition Set_Spring_Coefficient(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) f (X_IC st)
                (Xd_IC st) (out st).


Definition Set_X_IC(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st)
                (Time st) (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) f
                (Xd_IC st) (out st).

Definition Set_Xd_IC(st:State) : float -> State :=
  fun f:float => mkState
                (x st) (xDot st) 
                (Time st) (Time0 st) (Tstop st)
                (Dt st) (Damping_Coefficient st) (Gravity st)
                (Mass st) (Spring_Coefficient st) (X_IC st)
                f (out st).
Open Scope string_scope.
Definition print(st:State)(s:string) : State :=
  mkState
    (x st) (xDot st) 
    (Time st) (Time0 st) (Tstop st)
    (Dt st) (Damping_Coefficient st) (Gravity st)
    (Mass st) (Spring_Coefficient st) (X_IC st)
    (Xd_IC st)
    (match (out st) with
     |"" => s
     |record => record ++ newline ++ s
     end).