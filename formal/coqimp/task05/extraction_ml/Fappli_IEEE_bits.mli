open BinNums
open Datatypes
open Fappli_IEEE
open Fcore_Zaux

type binary64 = binary_float

val default_nan_pl64 : bool * nan_pl

val unop_nan_pl64 : binary64 -> bool * nan_pl

val b64_sqrt : mode -> binary_float -> binary_float
