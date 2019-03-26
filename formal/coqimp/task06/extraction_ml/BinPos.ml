open BinNums
open BinPosDef
open Datatypes
open Decimal

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | Coq_xI p -> Coq_xO (succ p)
  | Coq_xO p -> Coq_xI p
  | Coq_xH -> Coq_xO Coq_xH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> Coq_xO (add_carry p q)
       | Coq_xO q -> Coq_xI (add p q)
       | Coq_xH -> Coq_xO (succ p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> Coq_xI (add p q)
       | Coq_xO q -> Coq_xO (add p q)
       | Coq_xH -> Coq_xI p)
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Coq_xO (succ q)
       | Coq_xO q -> Coq_xI q
       | Coq_xH -> Coq_xO Coq_xH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> Coq_xI (add_carry p q)
       | Coq_xO q -> Coq_xO (add_carry p q)
       | Coq_xH -> Coq_xI (succ p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> Coq_xO (add_carry p q)
       | Coq_xO q -> Coq_xI (add p q)
       | Coq_xH -> Coq_xO (succ p))
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Coq_xI (succ q)
       | Coq_xO q -> Coq_xO (succ q)
       | Coq_xH -> Coq_xI Coq_xH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | Coq_xI p -> Coq_xI (Coq_xO p)
  | Coq_xO p -> Coq_xI (pred_double p)
  | Coq_xH -> Coq_xH

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos Coq_xH
  | IsPos p -> IsPos (Coq_xI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (Coq_xO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | Coq_xI p -> IsPos (Coq_xO (Coq_xO p))
  | Coq_xO p -> IsPos (Coq_xO (pred_double p))
  | Coq_xH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> double_mask (sub_mask p q)
       | Coq_xO q -> succ_double_mask (sub_mask p q)
       | Coq_xH -> IsPos (Coq_xO p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xO q -> double_mask (sub_mask p q)
       | Coq_xH -> IsPos (pred_double p))
    | Coq_xH -> (match y with
                 | Coq_xH -> IsNul
                 | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xO q -> double_mask (sub_mask p q)
       | Coq_xH -> IsPos (pred_double p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> double_mask (sub_mask_carry p q)
       | Coq_xO q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xH -> double_pred_mask p)
    | Coq_xH -> IsNeg

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | Coq_xI p -> add y (Coq_xO (mul p y))
    | Coq_xO p -> Coq_xO (mul p y)
    | Coq_xH -> y

  (** val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1 **)

  let rec iter f x = function
  | Coq_xI n' -> f (iter f (iter f x n') n')
  | Coq_xO n' -> iter f (iter f x n') n'
  | Coq_xH -> f x

  (** val square : positive -> positive **)

  let rec square = function
  | Coq_xI p0 -> Coq_xI (Coq_xO (add (square p0) p0))
  | Coq_xO p0 -> Coq_xO (Coq_xO (square p0))
  | Coq_xH -> Coq_xH

  (** val div2 : positive -> positive **)

  let div2 = function
  | Coq_xI p0 -> p0
  | Coq_xO p0 -> p0
  | Coq_xH -> Coq_xH

  (** val div2_up : positive -> positive **)

  let div2_up = function
  | Coq_xI p0 -> succ p0
  | Coq_xO p0 -> p0
  | Coq_xH -> Coq_xH

  (** val size : positive -> positive **)

  let rec size = function
  | Coq_xI p0 -> succ (size p0)
  | Coq_xO p0 -> succ (size p0)
  | Coq_xH -> Coq_xH

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> compare_cont r p q
       | Coq_xO q -> compare_cont Gt p q
       | Coq_xH -> Gt)
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> compare_cont Lt p q
       | Coq_xO q -> compare_cont r p q
       | Coq_xH -> Gt)
    | Coq_xH -> (match y with
                 | Coq_xH -> r
                 | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | Coq_xI p0 -> (match q with
                    | Coq_xI q0 -> eqb p0 q0
                    | _ -> false)
    | Coq_xO p0 -> (match q with
                    | Coq_xO q0 -> eqb p0 q0
                    | _ -> false)
    | Coq_xH -> (match q with
                 | Coq_xH -> true
                 | _ -> false)

  (** val leb : positive -> positive -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val sqrtrem_step :
      (positive -> positive) -> (positive -> positive) -> (positive * mask)
      -> positive * mask **)

  let sqrtrem_step f g = function
  | (s, y) ->
    (match y with
     | IsPos r ->
       let s' = Coq_xI (Coq_xO s) in
       let r' = g (f r) in
       if leb s' r'
       then ((Coq_xI s), (sub_mask r' s'))
       else ((Coq_xO s), (IsPos r'))
     | _ -> ((Coq_xO s), (sub_mask (g (f Coq_xH)) (Coq_xO (Coq_xO Coq_xH)))))

  (** val sqrtrem : positive -> positive * mask **)

  let rec sqrtrem = function
  | Coq_xI p0 ->
    (match p0 with
     | Coq_xI p1 ->
       sqrtrem_step (fun x -> Coq_xI x) (fun x -> Coq_xI x) (sqrtrem p1)
     | Coq_xO p1 ->
       sqrtrem_step (fun x -> Coq_xO x) (fun x -> Coq_xI x) (sqrtrem p1)
     | Coq_xH -> (Coq_xH, (IsPos (Coq_xO Coq_xH))))
  | Coq_xO p0 ->
    (match p0 with
     | Coq_xI p1 ->
       sqrtrem_step (fun x -> Coq_xI x) (fun x -> Coq_xO x) (sqrtrem p1)
     | Coq_xO p1 ->
       sqrtrem_step (fun x -> Coq_xO x) (fun x -> Coq_xO x) (sqrtrem p1)
     | Coq_xH -> (Coq_xH, (IsPos Coq_xH)))
  | Coq_xH -> (Coq_xH, IsNul)

  (** val coq_lor : positive -> positive -> positive **)

  let rec coq_lor p q =
    match p with
    | Coq_xI p0 ->
      (match q with
       | Coq_xI q0 -> Coq_xI (coq_lor p0 q0)
       | Coq_xO q0 -> Coq_xI (coq_lor p0 q0)
       | Coq_xH -> p)
    | Coq_xO p0 ->
      (match q with
       | Coq_xI q0 -> Coq_xI (coq_lor p0 q0)
       | Coq_xO q0 -> Coq_xO (coq_lor p0 q0)
       | Coq_xH -> Coq_xI p0)
    | Coq_xH -> (match q with
                 | Coq_xO q0 -> Coq_xI q0
                 | _ -> q)

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> Coq_xH
  | S x -> succ (of_succ_nat x)

  (** val of_uint_acc : uint -> positive -> positive **)

  let rec of_uint_acc d acc =
    match d with
    | Nil -> acc
    | D0 l -> of_uint_acc l (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc)
    | D1 l ->
      of_uint_acc l (add Coq_xH (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D2 l ->
      of_uint_acc l
        (add (Coq_xO Coq_xH) (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D3 l ->
      of_uint_acc l
        (add (Coq_xI Coq_xH) (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D4 l ->
      of_uint_acc l
        (add (Coq_xO (Coq_xO Coq_xH))
          (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D5 l ->
      of_uint_acc l
        (add (Coq_xI (Coq_xO Coq_xH))
          (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D6 l ->
      of_uint_acc l
        (add (Coq_xO (Coq_xI Coq_xH))
          (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D7 l ->
      of_uint_acc l
        (add (Coq_xI (Coq_xI Coq_xH))
          (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D8 l ->
      of_uint_acc l
        (add (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
          (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))
    | D9 l ->
      of_uint_acc l
        (add (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
          (mul (Coq_xO (Coq_xI (Coq_xO Coq_xH))) acc))

  (** val of_uint : uint -> coq_N **)

  let rec of_uint = function
  | Nil -> N0
  | D0 l -> of_uint l
  | D1 l -> Npos (of_uint_acc l Coq_xH)
  | D2 l -> Npos (of_uint_acc l (Coq_xO Coq_xH))
  | D3 l -> Npos (of_uint_acc l (Coq_xI Coq_xH))
  | D4 l -> Npos (of_uint_acc l (Coq_xO (Coq_xO Coq_xH)))
  | D5 l -> Npos (of_uint_acc l (Coq_xI (Coq_xO Coq_xH)))
  | D6 l -> Npos (of_uint_acc l (Coq_xO (Coq_xI Coq_xH)))
  | D7 l -> Npos (of_uint_acc l (Coq_xI (Coq_xI Coq_xH)))
  | D8 l -> Npos (of_uint_acc l (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
  | D9 l -> Npos (of_uint_acc l (Coq_xI (Coq_xO (Coq_xO Coq_xH))))

  (** val eq_dec : positive -> positive -> bool **)

  let rec eq_dec p x0 =
    match p with
    | Coq_xI p0 -> (match x0 with
                    | Coq_xI p1 -> eq_dec p0 p1
                    | _ -> false)
    | Coq_xO p0 -> (match x0 with
                    | Coq_xO p1 -> eq_dec p0 p1
                    | _ -> false)
    | Coq_xH -> (match x0 with
                 | Coq_xH -> true
                 | _ -> false)
 end
