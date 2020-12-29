Require Import digsim.State.
Require Import digsim.Advance_States.
Require Import digsim.Default_Data.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Lists.List.
Require Import Coq.ZArith.ZArith.
Require Import Task.float_text_io.
Require Import digsim.Events.

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

Definition clean (s:string) : string := reverseString (removeLeading (reverseString (removeLeading (stringMap capitalize s)))).

Definition prepare (f:list (list string)) : list (list string) := map (map clean) (stripComments f).

Definition execute (cmd : string)(args : list string)(st:State) : State + string :=
  if cmd =? "RUN"
  then inl (run st)
  else

  if cmd =? "STOP"
  then inl st

  else executeEvent cmd args st.

Search (Floats.float -> string).

Fixpoint statefulParser(input : list (list string))(st:State)(error:string) : State * string :=
  match input with
  |nil => (st, error)
  |line :: ls => match line with
               |nil => statefulParser ls st error
               |cmd :: cs => match cmd =? "STOP" with
                           |true => ((print st (cmd ++ "@" ++ FloatIO.float_to_string 4 3 (Time st))), error)
                           |false => match (execute cmd cs (print st ("Instruction: " ++ cmd ++ "@" ++ FloatIO.float_to_string 4 3 (Time st)))) with
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
 