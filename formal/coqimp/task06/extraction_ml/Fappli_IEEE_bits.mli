open BinInt
open BinNums
open Datatypes
open Fappli_IEEE
open Fcore_Zaux
open Zbool

val join_bits : coq_Z -> coq_Z -> bool -> coq_Z -> coq_Z -> coq_Z

val split_bits : coq_Z -> coq_Z -> coq_Z -> (bool * coq_Z) * coq_Z

val bits_of_binary_float : coq_Z -> coq_Z -> binary_float -> coq_Z

val binary_float_of_bits_aux : coq_Z -> coq_Z -> coq_Z -> full_float

val binary_float_of_bits : coq_Z -> coq_Z -> coq_Z -> binary_float

type binary64 = binary_float

val default_nan_pl64 : bool * nan_pl

val unop_nan_pl64 : binary64 -> bool * nan_pl

val b64_sqrt : mode -> binary_float -> binary_float

val b64_of_bits : coq_Z -> binary64

val bits_of_b64 : binary64 -> coq_Z
