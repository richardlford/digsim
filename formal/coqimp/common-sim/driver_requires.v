(** DigSim

   DigSim provides a Coq architecture required to simulate continuous systems
   described by sets of simultaneous first-order differential equations.

     xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;


 ** _CoqProject config file contents

   -R ~/opt/lib/compcert/coq compcert -R . Top

 *)

From RecordUpdate Require Export RecordUpdate.
Import RecordSetNotations.

Require Export Task.float_text_io.
Import FloatIO.
Require Export Task.debug_printers.
Import DebugIO.

Require Export Task.monad.
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

(*+ Handy Constants *)

Require Export Coq.Floats.Floats.

(* pi matching Haskell Prelude Double *)
Definition pi := 3.141592653589793238%float.

(* small floating point constant *)
Definition small := 0.000001%float.

(* comparison floating point constant *)
Definition epsilon := 0.0000000001%float.
