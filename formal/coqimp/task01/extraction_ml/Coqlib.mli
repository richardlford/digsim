open BinInt
open BinNums
open ZArith_dec

val zeq : coq_Z -> coq_Z -> bool

val zlt : coq_Z -> coq_Z -> bool

val zle : coq_Z -> coq_Z -> bool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option
