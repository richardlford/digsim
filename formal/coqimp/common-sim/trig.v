(** Trig

   Declare floating trigonometry function. These will
   later be extracted to OCaml or Haskell functions.
 *)

Require Import Coq.Floats.Floats. (* Coq primitive floats *)

Module CoqTrig.
Parameter pow : float -> float -> float.
Parameter sqrt : float -> float.
Parameter exp : float -> float.
Parameter log : float -> float.
Parameter log10 : float -> float.
Parameter expm1 : float -> float.
Parameter log1p : float -> float.
Parameter cos : float -> float.
Parameter sin : float -> float.
Parameter tan : float -> float.
Parameter acos : float -> float.
Parameter asin : float -> float.
Parameter atan : float -> float.
Parameter atan2 : float -> float -> float.
Parameter hypot : float -> float -> float.
Parameter cosh : float -> float.
Parameter sinh : float -> float.
Parameter tanh : float -> float.
Parameter ceil : float -> float.
Parameter floor : float -> float.
Parameter copysign : float -> float -> float.
End CoqTrig.
