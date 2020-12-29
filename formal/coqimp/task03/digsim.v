From compcert Require Import Floats.
From Coq Require Import Lists.List ZArith.ZArith Vectors.Vector Strings.Ascii Strings.String.

From digsim Require Import Advance_States Default_Data Differential_Equations State Termination_Conditions InputData.

Definition numberOfSteps : nat := 251.
Open Scope string_scope.

Definition File := list (list string).

Definition initializeState(input:File) : State := match (parser input)  with
                                      |inl st => st
                                      |inr s => (print (print Default_Data s) "Errors Found; using Default Data")
                                      end.

Fixpoint simulateNSteps(n:nat)(input:File) : State :=
match n with 
 |O =>  initializeState input
 |S k => advance (simulateNSteps k input)
end.

Fixpoint graph'(st:State)(ps:list (float * float))(index:_)(fuel:nat) : State * (list (float * float))%type :=
match fuel with 
 |O => (st, ps)
 |S k => match Quit st with
        |true => (st, ps)
        |false => graph' (advance st) ((Time st, (Vector.nth (x st) index)) :: ps) index k
        end
end.

Definition graph(index:_)(input:File) : list (float * float) := snd (graph' (initializeState input) List.nil index numberOfSteps).

Fixpoint graphxDot'(st:State)(ps:list (float * float))(index:_)(fuel:nat) : State * (list (float * float))%type :=
match fuel with 
 |O => (st, ps)
 |S k => match Quit st with
        |true => (st, ps)
        |false => graphxDot' (advance st) ((Time st, (Vector.nth (xDot st) index)) :: ps) index k
        end
end.

Definition graphxDot(index:_)(input:File) : list (float * float) := snd (graphxDot' (initializeState input) List.nil index numberOfSteps).
                                                                                        
Definition graph1(input:File) : list (float * float) := graph Fin.F1 input.
Definition graph2(input:File) : list (float * float)  := graphxDot Fin.F1 input.

Fixpoint dataTable (xs xDs : list (float * float)) : list (list float) :=
  match xs with
  |List.nil => List.nil
  |(t, x) :: xtl => match xDs with
                  |List.nil => List.nil
                  |(t0,xd) :: xdtl => (t :: x :: xd :: List.nil) :: (dataTable xtl xdtl)
                  end
  end.

Definition graphData(input:File) : list (list float) := dataTable (graph1 input) (graph2 input).
Definition dataComments(input:File) : string := (out (fst (graph' (initializeState input) List.nil Fin.F1 numberOfSteps))).