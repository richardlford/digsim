
val xorb : bool -> bool -> bool

val negb : bool -> bool

type nat =
| O
| S of nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val coq_CompOpp : comparison -> comparison
