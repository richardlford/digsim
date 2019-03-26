open BinNums
open Datatypes
open Fappli_IEEE
open Fcore_Zaux

type binary64 = binary_float

(** val default_nan_pl64 : bool * nan_pl **)

let default_nan_pl64 =
  (false,
    (iter_nat (fun x -> Coq_xO x) (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S
      O))))))))))))))))))))))))))))))))))))))))))))))))))) Coq_xH))

(** val unop_nan_pl64 : binary64 -> bool * nan_pl **)

let unop_nan_pl64 = function
| B754_nan (s, pl) -> (s, pl)
| _ -> default_nan_pl64

(** val b64_sqrt : mode -> binary_float -> binary_float **)

let b64_sqrt =
  coq_Bsqrt (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))))))))) unop_nan_pl64
