Require Import digsim.State.
Require Import digsim.Advance_States.
Require Import digsim.Default_Data.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Lists.List.
Require Import Coq.ZArith.ZArith.
Require Import Task.float_text_io.

Open Scope string_scope.

Open Scope Z_scope.

Fixpoint stripCommentsToken(s:string)(parens:Z) : string :=
  match s with
  |"" => ""
  |String hd tl => match hd with
                  |"("%char => stripCommentsToken tl (parens + 1)
                  |")"%char => stripCommentsToken tl (parens - 1)
                  | _ => match parens with
                              |Z0 => String hd (stripCommentsToken tl parens)
                              |_ => stripCommentsToken tl parens
                        end
                  end
  end.

Fixpoint countParensToken(s:string)(p:Z) : Z :=
  match s with
  |"" => p
  |String hd tl => match hd with
                  |"("%char => countParensToken tl (p+1)
                  |")"%char => countParensToken tl (p-1)
                  | _ => countParensToken tl p
                  end
  end.

Definition countParensLine(ss:list string)(parens:Z) : Z := fold_right Z.add Z0 (map (fun x:_ => countParensToken x parens) ss).

Fixpoint stripCommentsLine(ss:list string)(parens:Z) : (list string) :=
  match ss with
  |nil => nil
  |cons hd tl => match stripCommentsToken hd parens with
                  |"" => stripCommentsLine tl (countParensToken hd parens) 
                  |s => (s :: (stripCommentsLine tl (countParensToken hd parens)))
                end
  end.

Fixpoint stripCommentsFile(ss:list (list string))(parens:Z) : (list (list string)) :=
  match ss with
  |nil => nil
  |cons hd tl => match stripCommentsLine hd parens with
                |nil => stripCommentsFile tl (countParensLine hd parens)
                |s => (s :: (stripCommentsFile tl (countParensLine hd parens)))
                end
  end.

Definition countParensFile(ss:list (list string))(parens:Z) : Z := fold_right Z.add Z0 (map (fun x:_ => countParensLine x parens) ss).

Definition stripComments(ss:list (list string)) : list (list string) := (stripCommentsFile ss 0).

Open Scope string_scope.

Fixpoint endsWith (s:string) (c:ascii) : bool := match s with
                                             |"" => false
                                             |String a "" => (a =? c)%char
                                             |String a tl => (endsWith tl c)
                                              end.

Definition startsWith (s:string) (c:ascii) : bool := match s with
                                                  |String a tl => (a =? c)%char
                                                  |"" => false
                                                  end.

Fixpoint removeLeading (s:string) : string :=
  match s with
  |EmptyString => EmptyString
  |String hd tl => match hd with
                  |" "%char  => removeLeading tl
                  |"009"%char  => removeLeading tl
                  |"010"%char  => removeLeading tl
                  |"011"%char  => removeLeading tl
                  |"012"%char  => removeLeading tl
                  |"013"%char  => removeLeading tl
                  |_ => s
                  end
  end.

Theorem works : forall s:string, (removeLeading s) = (removeLeading (" " ++ s)).
  vm_compute.
trivial.  
Defined.

Open Scope char_scope.
Definition isLetter (c:ascii) : bool :=
  match c with
  |"@" => false
  |"`" => false
  |Ascii a b c d false f true false => true
  |Ascii a b c false true f true false => true
  |"x" => true
  |"X" => true
  |"y" => true
  |"Y" => true
  |"z" => true
  |"Z" => true
  |_ => false
  end.

Close Scope char_scope.

Definition  capitalize (c:ascii) : ascii :=
  if isLetter c
  then
    match c with
    |Ascii a b c d e f g h => Ascii a b c d e false g h
    end
  else
    c.

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

Theorem alphabet : stringMap capitalize "abcdefghijklmnopqrstuvwxyz" = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
  vm_compute.
  reflexivity.
Defined.

Theorem notalpha : stringMap capitalize "`~!@#$%^&*()_+-={}|[]\:"";'<>?,./" =  "`~!@#$%^&*()_+-={}|[]\:"";'<>?,./".
  vm_compute.
  reflexivity.
  Defined.

Theorem nums : stringMap capitalize "0123456789" = "0123456789".
  vm_compute.
  reflexivity.
  Defined.

Definition reverseString (s:string) : string := listToString (rev (stringToList s)).

Definition clean (s:string) : string := reverseString (removeLeading (reverseString (removeLeading (stringMap capitalize s)))).

Definition prepare (f:list (list string)) : list (list string) := map (map clean) (stripComments f).
  
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

Open Scope string_scope.

Definition getArrayField(f:string)(i:Fin.t Ndes)(st:State) : option string :=
  if f =? "X" then Some (FloatIO.float_to_string 53 1024 (Vector.nth (x st) i)) else
  if f =? "XD" then Some (FloatIO.float_to_string 53 1024 (Vector.nth (xDot st) i)) else
    None.

Definition getField(f:string)(st:State) : option string :=
  if f =? "TIME" then Some (FloatIO.float_to_string 4 3 (Time st)) else
  if f =? "TIME0" then Some (FloatIO.float_to_string 4 3 (Time0 st)) else
  if f =? "TSTOP" then Some (FloatIO.float_to_string 4 3 (Tstop st)) else
  if f =? "DT" then Some (FloatIO.float_to_string 4 3 (Dt st)) else
  if f =? "DAMPING" then Some (FloatIO.float_to_string 4 3 (Damping_Coefficient st)) else
  if f =? "GRAVITY" then Some (FloatIO.float_to_string 4 3 (Gravity st)) else
  if f =? "MASS" then Some (FloatIO.float_to_string 4 3 (Mass st)) else
  if f =? "SPRING" then Some (FloatIO.float_to_string 4 3 (Spring_Coefficient  st)) else
    None.

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
  if f =? "DT" then Some (Set_Dt st) else
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

Definition execute (cmd : string)(args : list string)(st:State) : State + string :=
  if cmd =? "PRINT"
  then printer args st
  else

  if cmd =? "SET"
  then setter args st
  else

  if cmd =? "RUN"
  then inl (run st)
  else

  if cmd =? "STOP"
  then inl st

  else inr ("ERR99: Unrecognized Command: " ++ cmd ++ " " ++ (fold_right (fun x:_ => fun y:_ => x ++ " " ++ y) "" args)).

Fixpoint statefulParser(input : list (list string))(st:State)(error:string) : State * string :=
  match input with
  |nil => (st, error)
  |line :: ls => match line with
               |nil => statefulParser ls st error
               |cmd :: cs => match cmd =? "STOP" with
                           |true => (st, error)
                           |false => match (execute cmd cs st) with
                                    |inl advSt => statefulParser ls advSt error
                                    |inr e => (st, e ++ newline ++ error)
                                    end
                           end
               end
  end.

  Definition parser(input : list (list string)) : State + string :=
    match (statefulParser (prepare input) Default_Data "") with
    |(st, "") => inl st
    |(_, err) => inr err
    end.
 