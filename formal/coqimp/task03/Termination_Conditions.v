From compcert Require Import Floats.

Require Import digsim.State.

 Definition Quit(st:State) : bool := Float.cmp Integers.Cgt (Time st) (Tstop st).
 