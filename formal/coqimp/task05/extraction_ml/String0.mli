open Datatypes

val eqb : char list -> char list -> bool

val append : char list -> char list -> char list

val length : char list -> nat

val substring : nat -> nat -> char list -> char list

val prefix : char list -> char list -> bool

val index : nat -> char list -> char list -> nat option
