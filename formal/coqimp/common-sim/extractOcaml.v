Require Export Task.driver3.
Require Import DigUty.debug_printers.
Import DebugIO.
Import Maps.

Extraction Language OCaml.
(* Unset Extraction Optimize. *)
(* Unset Extraction AutoInline. *)

(* Standard lib *)
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import DigUty.ExtrOcamlInt64Conv.

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
         nat_of_int64 pos_of_int64 z_of_int64 n_of_int64
         Float.to_bits Float.of_bits.


Cd "..".
