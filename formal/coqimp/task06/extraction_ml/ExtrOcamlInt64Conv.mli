open BinNums
open Datatypes

val int64_of_nat : nat -> int64

val int64_of_pos : positive -> int64

val int64_of_z : coq_Z -> int64

val int64_of_n : coq_N -> int64

val int64_natlike_rec : 'a1 -> ('a1 -> 'a1) -> int64 -> 'a1

val nat_of_int64 : int64 -> nat

val int64_poslike_rec : 'a1 -> ('a1 -> 'a1) -> ('a1 -> 'a1) -> int64 -> 'a1

val pos_of_int64 : int64 -> positive

val int64_zlike_case : 'a1 -> (int64 -> 'a1) -> (int64 -> 'a1) -> int64 -> 'a1

val z_of_int64 : int64 -> coq_Z

val n_of_int64 : int64 -> coq_N
