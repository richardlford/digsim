From compcert Require Import Floats.
Require Import digsim.State.
Require Import Coq.QArith.QArith.
Require Import Coq.Vectors.Vector.

Definition Xdd (st:State) : t float Ndes := map2 (fun x:float => fun xd:float => Float.sub (Float.div (Float.neg (Float.add (Float.mul (Spring_Coefficient st) (x)) (Float.mul (Damping_Coefficient st) (xd)))) (Mass st)) (Gravity st)) (x st) (xDot st).
