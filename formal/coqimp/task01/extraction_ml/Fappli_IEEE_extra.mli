open BinInt
open BinNums
open BinPos
open Datatypes
open Fappli_IEEE
open Fcore_Zaux

val coq_BofZ : coq_Z -> coq_Z -> coq_Z -> binary_float

val coq_ZofB : coq_Z -> coq_Z -> binary_float -> coq_Z option

val coq_ZofB_range : coq_Z -> coq_Z -> binary_float -> coq_Z -> coq_Z -> coq_Z option

val pos_pow : positive -> positive -> positive

val coq_Bparse : coq_Z -> coq_Z -> positive -> positive -> coq_Z -> binary_float
