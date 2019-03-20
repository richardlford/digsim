Require Import DigUty.float_text_io.
Import FloatIO.
Require Import DigUty.debug_printers.
Import DebugIO.

(* Standard lib *)
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

(* Coqlib *)
Extract Inlined Constant Coqlib.proj_sumbool => "(fun x -> x)".

(* Datatypes *)
Extract Inlined Constant Datatypes.fst => "fst".
Extract Inlined Constant Datatypes.snd => "snd".

(* Avoid name clashes *)
Extraction Blacklist List String Int.

(* Cutting the dependency to R. *)
Extract Inlined Constant Fcore_defs.F2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.FF2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.B2R => "fun _ -> assert false".
Extract Inlined Constant Fappli_IEEE.round_mode => "fun _ -> assert false".
Extract Inlined Constant Fcalc_bracket.inbetween_loc => "fun _ -> assert false".
(* Go! *)

Cd "extraction".
Separate Extraction Z_to_float nat_power_of_float Z_to_string_base10 pad_to_width float_to_string
           strToFloat strToFloat' print_float print_Z.

Cd "..".
