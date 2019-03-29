Require Export Task.driver_run.
Require Import Task.debug_printers.
Import DebugIO.
Import Maps.

Extraction Language Haskell.
(* Unset Extraction Optimize. *)
(* Unset Extraction AutoInline. *)

(* Standard lib *)
Require Import ExtrHaskellBasic.
Require Import ExtrHaskellString.
Require Import ExtrHaskellNatInteger.
Require Import ExtrHaskellZInteger.
Require Import ExtrHaskellNatNum.

(* Coqlib *)
Extract Inlined Constant Coqlib.proj_sumbool => "(fun x -> x)".

(* Datatypes *)
Extract Inlined Constant Datatypes.fst => "fst".
Extract Inlined Constant Datatypes.snd => "snd".

(* Decidable *)

Extraction Inline DecidableClass.Decidable_witness DecidableClass.decide
   Decidableplus.Decidable_and Decidableplus.Decidable_or
   Decidableplus.Decidable_not Decidableplus.Decidable_implies.

(* Avoid name clashes *)
Extraction Blacklist List String Int.

(* Cutting the dependency to R. *)
Extract Inlined Constant Fcore_defs.F2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.FF2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.B2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.round_mode => "fun _ -> assert false".
Extract Inlined Constant Fcalc_bracket.inbetween_loc => "fun _ -> assert false".
(* Go! *)

Cd "extraction_hs".

Separate Extraction main print_Z svToStr posToStateVar' PTree.elements.

Cd "..".
