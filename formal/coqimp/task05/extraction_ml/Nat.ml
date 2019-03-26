open Datatypes

(** val pred : nat -> nat **)

let pred n = match n with
| O -> n
| S u -> u

(** val sub : nat -> nat -> nat **)

let rec sub n m =
  match n with
  | O -> n
  | S k -> (match m with
            | O -> n
            | S l -> sub k l)
