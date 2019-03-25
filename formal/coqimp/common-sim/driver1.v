(** DigSim

   DigSim provides a Coq architecture required to simulate continuous systems
   described by sets of simultaneous first-order differential equations.

     xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;


 ** _CoqProject config file contents

   -R ~/opt/lib/compcert/coq compcert -R . Top

 *)

From RecordUpdate Require Export RecordUpdate.
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

Require Export DigUty.float_text_io.
Import FloatIO.
Require Export DigUty.debug_printers.
Import DebugIO.

Require Export DigUty.Monad.

From compcert Require Export Floats.
Import Float.
From compcert Require Export Fappli_IEEE.
From compcert Require Export Integers.
Require Export Coq.Lists.List.

Import ListNotations.

Require Export BinNums.
Require Export Coq.QArith.QArith_base.
Require Export Strings.String.
Require Export Strings.Ascii.
Require Export Coq.ZArith.Znat.
Require Export Recdef.
From compcert Require Export Maps.

Require Export Zwf.
From compcert Require Export Coqlib.

