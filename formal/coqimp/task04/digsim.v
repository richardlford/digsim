From compcert Require Import Floats.
From Coq Require Import Lists.List ZArith.ZArith Vectors.Vector Strings.Ascii Strings.String.

From digsim Require Import Advance_States Default_Data Differential_Equations State Termination_Conditions InputData.

Open Scope string_scope.

Definition File := list (list string).

Definition initializeState(input:File) : State := match (parser input)  with
                                      |inl st => st
                                      |inr s => (print (print Default_Data s) "Errors Found; using Default Data")
                                      end.

Definition dataComments(input:File) : string := (out (initializeState input)).