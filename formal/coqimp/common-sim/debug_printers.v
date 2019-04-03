(* Functions to print common data types. *)
Require Import Task.float_text_io.

Import FloatIO.

Module DebugIO.
Definition width := 22%nat.
Definition fdigs := 16%nat.
Definition print_float := float_to_string width fdigs.
Definition print_Z := Z_to_string_base10 1%nat.

End DebugIO.
