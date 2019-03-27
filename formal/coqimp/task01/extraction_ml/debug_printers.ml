open BinNums
open Datatypes
open Floats
open Float_text_io

module DebugIO =
 struct
  (** val width : nat **)

  let width =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S O)))))))))))))

  (** val fdigs : nat **)

  let fdigs =
    S (S (S (S (S (S (S (S (S O))))))))

  (** val print_float : float -> char list **)

  let print_float =
    FloatIO.float_to_string width fdigs

  (** val print_Z : coq_Z -> char list **)

  let print_Z =
    FloatIO.coq_Z_to_string_base10 (S O)
 end
