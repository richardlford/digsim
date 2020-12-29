Require Import digsim.State.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Lists.List.
Require Import Coq.ZArith.ZArith.
Require Import Task.float_text_io.

Open Scope string_scope.

Definition getArrayField(f:string)(i:Fin.t Ndes)(st:State) : option string :=
  if f =? "X" then Some (FloatIO.float_to_string 53 1024 (Vector.nth (x st) i)) else
  if f =? "XD" then Some (FloatIO.float_to_string 53 1024 (Vector.nth (xDot st) i)) else
    None.

Definition getField(f:string)(st:State) : option string :=
  if f =? "TIME" then Some (FloatIO.float_to_string 4 3 (Time st)) else
  if f =? "TIME0" then Some (FloatIO.float_to_string 4 3 (Time0 st)) else
  if f =? "TSTOP" then Some (FloatIO.float_to_string 4 3 (Tstop st)) else
  if f =? "DT" then Some (FloatIO.float_to_string 4 3 (DtMax st)) else
  if f =? "DAMPING" then Some (FloatIO.float_to_string 4 3 (Damping_Coefficient st)) else
  if f =? "GRAVITY" then Some (FloatIO.float_to_string 4 3 (Gravity st)) else
  if f =? "MASS" then Some (FloatIO.float_to_string 4 3 (Mass st)) else
  if f =? "SPRING" then Some (FloatIO.float_to_string 4 3 (Spring_Coefficient  st)) else
    None.
  
Open Scope nat_scope.
Fixpoint stringToNatHelper(s:string) : option nat :=
  match s with
  |"" => Some 0
  |String hd tl => match stringToNatHelper tl with
                  |None => None
                  |Some x => match hd with
                            |"0"%char => Some ((10 * x) + 0)
                            |"1"%char => Some ((10 * x) + 1)
                            |"2"%char => Some ((10 * x) + 2)
                            |"3"%char => Some ((10 * x) + 3)
                            |"4"%char => Some ((10 * x) + 4)
                            |"5"%char => Some ((10 * x) + 5)
                            |"6"%char => Some ((10 * x) + 6)
                            |"7"%char => Some ((10 * x) + 7)
                            |"8"%char => Some ((10 * x) + 8)
                            |"9"%char => Some ((10 * x) + 9)
                            |_ => None
                            end
                  end
  end.

Open Scope string_scope.

Fixpoint stringToList (s:string) : list ascii :=
  match s with
  |"" => nil
  |String hd tl => hd :: (stringToList tl)
  end.

Fixpoint listToString (s:list ascii) : string :=
  match s with
  |nil => ""
  |cons hd tl => String hd (listToString tl)
  end.

Definition stringMap (f:ascii -> ascii) (s:string) : string := listToString (map f (stringToList s)).

Definition reverseString (s:string) : string := listToString (rev (stringToList s)).

Definition stringToNat(s:string) := match s with
                                    |"" => None
                                    |x => stringToNatHelper (reverseString x)
                                    end.

Example stn : stringToNat "149" = Some 149.
vm_compute.
reflexivity.
Defined.

Definition getIndex(index:string) : option (Fin.t  Ndes) :=
  match  (stringToNat index) with
    |Some n =>   match (Fin.of_nat n Ndes) with
                |inleft x => Some x
                |inright _ => None
                end
    |None => None
  end.

Definition printer (args : list string)(st:State) : State + string :=
  match args with
  |f :: index :: nil => match getIndex index with
                     |Some i => match (getArrayField f i st) with
                               |Some x => inl (print st x)
                               |None =>  inr ("ERR01: Malformed Command: PRINT " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
                               end           
                     |None => inr ("ERR02: Malformed Command: PRINT " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
                     end
  |f :: nil => match (getField f st) with
             |Some x => inl (print st x)
             |None => inr ("ERR03: Malformed Command: PRINT " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
             end
  |_ =>  inr ("ERR04: Wrong number of arguments: PRINT " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))       
  end.

Definition setArrayField(f:string)(i:Fin.t Ndes)(st:State) : option (Floats.float -> State) :=
  if f =? "X" then Some (Set_x st i)  else
  if f =? "XD" then Some (Set_xDot st i) else
    None.

Definition setField(f:string)(st:State) : option (Floats.float -> State) :=
  if f =? "TIME" then Some (Set_Time st) else
  if f =? "TIME0" then Some (Set_Time0 st) else
  if f =? "TSTOP" then Some (Set_Tstop st) else
  if f =? "DT" then Some (Set_DtMax st) else
  if f =? "DAMPING" then Some (Set_Damping_Coefficient st) else
  if f =? "GRAVITY" then Some  (Set_Gravity st) else
  if f =? "MASS" then Some (Set_Mass st) else
  if f =? "SPRING" then Some (Set_Spring_Coefficient st) else
    None.

Definition setter (args : list string)(st:State) : State + string :=
  match args with
  |f :: index :: val :: nil => match getIndex index with
                     |Some i => match (setArrayField f i st) with
                               |Some func => inl (func (FloatIO.strToFloat val))
                               |None =>  inr ("ERR05: Malformed Command: SET " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
                               end           
                     |None => inr ("ERR06: Malformed Command: SET " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
                     end
  |f :: val :: nil => match (setField f st) with
             |Some func => inl (func (FloatIO.strToFloat val))
             |None => inr ("ERR07: Malformed Command: SET " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
             end
  |_ =>  inr ("ERR08: Wrong number of arguments: SET " ++  (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))       
  end.

Definition executeEvent (cmd : string)(args : list string)(st:State) : State + string :=
  if cmd =? "PRINT"
  then printer args st
  else

  if cmd =? "SET"
  then setter args st
  else

  if cmd =? "RUN"
  then inr "ERR10: Cannot run while already running."
  else

  if cmd =? "STOP"
  then inl st
  else
           
  if cmd =? "SCHEDULE"
  then match args with
       |timestr :: cmd' :: args' => inl (schedule st (FloatIO.strToFloat timestr) (cmd', args'))
       |_ =>  inr ("ERR11: Misformed Scheduling Command: " ++ cmd ++ " " ++ (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args))
       end
           
  else inr ("ERR99: Unrecognized Command: " ++ cmd ++ " " ++ (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args)).
