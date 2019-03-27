open BinInt
open BinNums
open ZArith_dec

(** val zeq : coq_Z -> coq_Z -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : coq_Z -> coq_Z -> bool **)

let zlt =
  coq_Z_lt_dec

(** val zle : coq_Z -> coq_Z -> bool **)

let zle =
  coq_Z_le_gt_dec

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None
