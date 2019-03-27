open Datatypes

(** val eqb : char list -> char list -> bool **)

let rec eqb s1 s2 =
  match s1 with
  | [] -> (match s2 with
           | [] -> true
           | _::_ -> false)
  | c1::s1' -> (match s2 with
                | [] -> false
                | c2::s2' -> if (=) c1 c2 then eqb s1' s2' else false)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val length : char list -> nat **)

let rec length = function
| [] -> O
| _::s' -> S (length s')

(** val substring : nat -> nat -> char list -> char list **)

let rec substring n m s =
  match n with
  | O -> (match m with
          | O -> []
          | S m' -> (match s with
                     | [] -> s
                     | c::s' -> c::(substring O m' s')))
  | S n' -> (match s with
             | [] -> s
             | _::s' -> substring n' m s')

(** val prefix : char list -> char list -> bool **)

let rec prefix s1 s2 =
  match s1 with
  | [] -> true
  | a::s1' -> (match s2 with
               | [] -> false
               | b::s2' -> if (=) a b then prefix s1' s2' else false)

(** val index : nat -> char list -> char list -> nat option **)

let rec index n s1 s2 = match s2 with
| [] -> (match n with
         | O -> (match s1 with
                 | [] -> Some O
                 | _::_ -> None)
         | S _ -> None)
| _::s2' ->
  (match n with
   | O -> if prefix s1 s2 then Some O else (match index O s1 s2' with
                                            | Some n0 -> Some (S n0)
                                            | None -> None)
   | S n' -> (match index n' s1 s2' with
              | Some n0 -> Some (S n0)
              | None -> None))
