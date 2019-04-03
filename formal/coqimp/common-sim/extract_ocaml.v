Require Export Task.driver_run.
Require Import Task.debug_printers.
Require Import Task.trig.
Require Import Task.extract_ocaml_trig.
Import DebugIO.
Import Maps.

Extraction Language OCaml.
(* Unset Extraction Optimize. *)
(* Unset Extraction AutoInline. *)

(* Standard lib *)
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Task.extr_ocaml_int64_conv.

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
Extraction Blacklist List String Int Int64.

(* Cutting the dependency to R. *)
Extract Inlined Constant Fcore_defs.F2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.FF2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.B2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.round_mode => "fun _ -> assert false".
Extract Inlined Constant Fcalc_bracket.inbetween_loc => "fun _ -> assert false".
(* Go! *)

Cd "extraction_ml".

Separate Extraction main print_Z svToStr posToStateVar' PTree.elements
         int64_of_nat int64_of_pos int64_of_z int64_of_n
         nat_of_int64 pos_of_int64 z_of_int64 z_of_uint64 n_of_int64
         Float.to_bits Float.of_bits
         CoqTrig.pow CoqTrig.sqrt CoqTrig.exp CoqTrig.log CoqTrig.log10
         CoqTrig.expm1 CoqTrig.log1p CoqTrig.cos CoqTrig.sin CoqTrig.tan
         CoqTrig.acos CoqTrig.asin CoqTrig.atan CoqTrig.atan2 CoqTrig.hypot
         CoqTrig.cosh CoqTrig.sinh CoqTrig.tanh CoqTrig.ceil CoqTrig.floor
         CoqTrig.copysign.


Cd "..".
