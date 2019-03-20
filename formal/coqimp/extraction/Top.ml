
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val xorb : bool -> bool -> bool **)

let xorb b1 b2 =
  if b1 then if b2 then false else true else b2

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type nat =
| O
| S of nat

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

type int =
| Pos of uint
| Neg of uint

(** val nzhead : uint -> uint **)

let rec nzhead d = match d with
| D0 d0 -> nzhead d0
| _ -> d

(** val unorm : uint -> uint **)

let unorm d =
  match nzhead d with
  | Nil -> D0 Nil
  | x -> x

(** val norm : int -> int **)

let norm = function
| Pos d0 -> Pos (unorm d0)
| Neg d0 -> (match nzhead d0 with
             | Nil -> Pos (D0 Nil)
             | x -> Neg x)

(** val pred : nat -> nat **)

let pred n0 = match n0 with
| O -> n0
| S u -> u

(** val sub : nat -> nat -> nat **)

let rec sub n0 m =
  match n0 with
  | O -> n0
  | S k -> (match m with
            | O -> n0
            | S l -> sub k l)

type ('e, 't) reader = 'e -> 't

(** val constructor : 'a2 -> ('a1, 'a2) reader **)

let constructor x _ =
  x

type ('r, 't) setter = ('t -> 't) -> 'r -> 'r
  (* singleton inductive, whose constructor was Build_Setter *)

(** val set : ('a1 -> 'a2) -> ('a1, 'a2) setter -> ('a2 -> 'a2) -> 'a1 -> 'a1 **)

let set _ setter0 =
  setter0

(** val eqb : bool -> bool -> bool **)

let eqb b1 b2 =
  if b1 then b2 else if b2 then false else true

module Nat =
 struct
  (** val leb : nat -> nat -> bool **)

  let rec leb n0 m =
    match n0 with
    | O -> true
    | S n' -> (match m with
               | O -> false
               | S m' -> leb n' m')

  (** val ltb : nat -> nat -> bool **)

  let ltb n0 m =
    leb (S n0) m
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p -> (match y with
               | XI q -> XO (add_carry p q)
               | XO q -> XI (add p q)
               | XH -> XO (succ p))
    | XO p -> (match y with
               | XI q -> XI (add p q)
               | XO q -> XO (add p q)
               | XH -> XI p)
    | XH -> (match y with
             | XI q -> XO (succ q)
             | XO q -> XI q
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p -> (match y with
               | XI q -> XI (add_carry p q)
               | XO q -> XO (add_carry p q)
               | XH -> XI (succ p))
    | XO p -> (match y with
               | XI q -> XO (add_carry p q)
               | XO q -> XI (add p q)
               | XH -> XO (succ p))
    | XH -> (match y with
             | XI q -> XI (succ q)
             | XO q -> XO (succ q)
             | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double_mask (sub_mask p q)
       | XO q -> succ_double_mask (sub_mask p q)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH -> (match y with
             | XH -> IsNul
             | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1 **)

  let rec iter f x = function
  | XI n' -> f (iter f (iter f x n') n')
  | XO n' -> iter f (iter f x n') n'
  | XH -> f x

  (** val square : positive -> positive **)

  let rec square = function
  | XI p0 -> XI (XO (add (square p0) p0))
  | XO p0 -> XO (XO (square p0))
  | XH -> XH

  (** val div2 : positive -> positive **)

  let div2 = function
  | XI p0 -> p0
  | XO p0 -> p0
  | XH -> XH

  (** val div2_up : positive -> positive **)

  let div2_up = function
  | XI p0 -> succ p0
  | XO p0 -> p0
  | XH -> XH

  (** val size : positive -> positive **)

  let rec size = function
  | XI p0 -> succ (size p0)
  | XO p0 -> succ (size p0)
  | XH -> XH

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p -> (match y with
               | XI q -> compare_cont r p q
               | XO q -> compare_cont Gt p q
               | XH -> Gt)
    | XO p -> (match y with
               | XI q -> compare_cont Lt p q
               | XO q -> compare_cont r p q
               | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> eqb p0 q0
                | _ -> false)
    | XO p0 -> (match q with
                | XO q0 -> eqb p0 q0
                | _ -> false)
    | XH -> (match q with
             | XH -> true
             | _ -> false)

  (** val leb : positive -> positive -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val sqrtrem_step :
      (positive -> positive) -> (positive -> positive) -> (positive * mask) -> positive * mask **)

  let sqrtrem_step f g = function
  | (s, y) ->
    (match y with
     | IsPos r ->
       let s' = XI (XO s) in
       let r' = g (f r) in if leb s' r' then ((XI s), (sub_mask r' s')) else ((XO s), (IsPos r'))
     | _ -> ((XO s), (sub_mask (g (f XH)) (XO (XO XH)))))

  (** val sqrtrem : positive -> positive * mask **)

  let rec sqrtrem = function
  | XI p0 ->
    (match p0 with
     | XI p1 -> sqrtrem_step (fun x -> XI x) (fun x -> XI x) (sqrtrem p1)
     | XO p1 -> sqrtrem_step (fun x -> XO x) (fun x -> XI x) (sqrtrem p1)
     | XH -> (XH, (IsPos (XO XH))))
  | XO p0 ->
    (match p0 with
     | XI p1 -> sqrtrem_step (fun x -> XI x) (fun x -> XO x) (sqrtrem p1)
     | XO p1 -> sqrtrem_step (fun x -> XO x) (fun x -> XO x) (sqrtrem p1)
     | XH -> (XH, (IsPos XH)))
  | XH -> (XH, IsNul)

  (** val coq_lor : positive -> positive -> positive **)

  let rec coq_lor p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> XI (coq_lor p0 q0)
                | XO q0 -> XI (coq_lor p0 q0)
                | XH -> p)
    | XO p0 -> (match q with
                | XI q0 -> XI (coq_lor p0 q0)
                | XO q0 -> XO (coq_lor p0 q0)
                | XH -> XI p0)
    | XH -> (match q with
             | XO q0 -> XI q0
             | _ -> q)

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)

  (** val of_uint_acc : uint -> positive -> positive **)

  let rec of_uint_acc d acc =
    match d with
    | Nil -> acc
    | D0 l -> of_uint_acc l (mul (XO (XI (XO XH))) acc)
    | D1 l -> of_uint_acc l (add XH (mul (XO (XI (XO XH))) acc))
    | D2 l -> of_uint_acc l (add (XO XH) (mul (XO (XI (XO XH))) acc))
    | D3 l -> of_uint_acc l (add (XI XH) (mul (XO (XI (XO XH))) acc))
    | D4 l -> of_uint_acc l (add (XO (XO XH)) (mul (XO (XI (XO XH))) acc))
    | D5 l -> of_uint_acc l (add (XI (XO XH)) (mul (XO (XI (XO XH))) acc))
    | D6 l -> of_uint_acc l (add (XO (XI XH)) (mul (XO (XI (XO XH))) acc))
    | D7 l -> of_uint_acc l (add (XI (XI XH)) (mul (XO (XI (XO XH))) acc))
    | D8 l -> of_uint_acc l (add (XO (XO (XO XH))) (mul (XO (XI (XO XH))) acc))
    | D9 l -> of_uint_acc l (add (XI (XO (XO XH))) (mul (XO (XI (XO XH))) acc))

  (** val of_uint : uint -> n **)

  let rec of_uint = function
  | Nil -> N0
  | D0 l -> of_uint l
  | D1 l -> Npos (of_uint_acc l XH)
  | D2 l -> Npos (of_uint_acc l (XO XH))
  | D3 l -> Npos (of_uint_acc l (XI XH))
  | D4 l -> Npos (of_uint_acc l (XO (XO XH)))
  | D5 l -> Npos (of_uint_acc l (XI (XO XH)))
  | D6 l -> Npos (of_uint_acc l (XO (XI XH)))
  | D7 l -> Npos (of_uint_acc l (XI (XI XH)))
  | D8 l -> Npos (of_uint_acc l (XO (XO (XO XH))))
  | D9 l -> Npos (of_uint_acc l (XI (XO (XO XH))))

  (** val eq_dec : positive -> positive -> bool **)

  let rec eq_dec p x0 =
    match p with
    | XI p0 -> (match x0 with
                | XI p1 -> eq_dec p0 p1
                | _ -> false)
    | XO p0 -> (match x0 with
                | XO p1 -> eq_dec p0 p1
                | _ -> false)
    | XH -> (match x0 with
             | XH -> true
             | _ -> false)
 end

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Coq_Pos.pred_double p)

  (** val pred_double : z -> z **)

  let pred_double = function
  | Z0 -> Zneg XH
  | Zpos p -> Zpos (Coq_Pos.pred_double p)
  | Zneg p -> Zneg (XI p)

  (** val pos_sub : positive -> positive -> z **)

  let rec pos_sub x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double (pos_sub p q)
       | XO q -> succ_double (pos_sub p q)
       | XH -> Zpos (XO p))
    | XO p ->
      (match y with
       | XI q -> pred_double (pos_sub p q)
       | XO q -> double (pos_sub p q)
       | XH -> Zpos (Coq_Pos.pred_double p))
    | XH -> (match y with
             | XI q -> Zneg (XO q)
             | XO q -> Zneg (Coq_Pos.pred_double q)
             | XH -> Z0)

  (** val add : z -> z -> z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' -> (match y with
                  | Z0 -> x
                  | Zpos y' -> Zpos (Coq_Pos.add x' y')
                  | Zneg y' -> pos_sub x' y')
    | Zneg x' -> (match y with
                  | Z0 -> x
                  | Zpos y' -> pos_sub y' x'
                  | Zneg y' -> Zneg (Coq_Pos.add x' y'))

  (** val opp : z -> z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val succ : z -> z **)

  let succ x =
    add x (Zpos XH)

  (** val pred : z -> z **)

  let pred x =
    add x (Zneg XH)

  (** val sub : z -> z -> z **)

  let sub m n0 =
    add m (opp n0)

  (** val mul : z -> z -> z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Coq_Pos.mul x' y')
       | Zneg y' -> Zneg (Coq_Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Coq_Pos.mul x' y')
       | Zneg y' -> Zpos (Coq_Pos.mul x' y'))

  (** val pow_pos : z -> positive -> z **)

  let pow_pos z0 =
    Coq_Pos.iter (mul z0) (Zpos XH)

  (** val pow : z -> z -> z **)

  let pow x = function
  | Z0 -> Zpos XH
  | Zpos p -> pow_pos x p
  | Zneg _ -> Z0

  (** val compare : z -> z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Coq_Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' -> (match y with
                  | Zneg y' -> compOpp (Coq_Pos.compare x' y')
                  | _ -> Lt)

  (** val leb : z -> z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : z -> z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val eqb : z -> z -> bool **)

  let rec eqb x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos p -> (match y with
                 | Zpos q -> Coq_Pos.eqb p q
                 | _ -> false)
    | Zneg p -> (match y with
                 | Zneg q -> Coq_Pos.eqb p q
                 | _ -> false)

  (** val max : z -> z -> z **)

  let max n0 m =
    match compare n0 m with
    | Lt -> m
    | _ -> n0

  (** val min : z -> z -> z **)

  let min n0 m =
    match compare n0 m with
    | Gt -> m
    | _ -> n0

  (** val of_nat : nat -> z **)

  let of_nat = function
  | O -> Z0
  | S n1 -> Zpos (Coq_Pos.of_succ_nat n1)

  (** val of_N : n -> z **)

  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p

  (** val of_uint : uint -> z **)

  let of_uint d =
    of_N (Coq_Pos.of_uint d)

  (** val of_int : int -> z **)

  let of_int = function
  | Pos d0 -> of_uint d0
  | Neg d0 -> opp (of_uint d0)

  (** val pos_div_eucl : positive -> z -> z * z **)

  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul (Zpos (XO XH)) r) (Zpos XH) in
      if ltb r' b then ((mul (Zpos (XO XH)) q), r') else ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b))
    | XO a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul (Zpos (XO XH)) r in
      if ltb r' b then ((mul (Zpos (XO XH)) q), r') else ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b))
    | XH -> if leb (Zpos (XO XH)) b then (Z0, (Zpos XH)) else ((Zpos XH), Z0)

  (** val div_eucl : z -> z -> z * z **)

  let div_eucl a b =
    match a with
    | Z0 -> (Z0, Z0)
    | Zpos a' ->
      (match b with
       | Z0 -> (Z0, Z0)
       | Zpos _ -> pos_div_eucl a' b
       | Zneg b' ->
         let (q, r) = pos_div_eucl a' (Zpos b') in
         (match r with
          | Z0 -> ((opp q), Z0)
          | _ -> ((opp (add q (Zpos XH))), (add b r))))
    | Zneg a' ->
      (match b with
       | Z0 -> (Z0, Z0)
       | Zpos _ ->
         let (q, r) = pos_div_eucl a' b in
         (match r with
          | Z0 -> ((opp q), Z0)
          | _ -> ((opp (add q (Zpos XH))), (sub b r)))
       | Zneg b' -> let (q, r) = pos_div_eucl a' (Zpos b') in (q, (opp r)))

  (** val div : z -> z -> z **)

  let div a b =
    let (q, _) = div_eucl a b in q

  (** val modulo : z -> z -> z **)

  let modulo a b =
    let (_, r) = div_eucl a b in r

  (** val div2 : z -> z **)

  let div2 = function
  | Z0 -> Z0
  | Zpos p -> (match p with
               | XH -> Z0
               | _ -> Zpos (Coq_Pos.div2 p))
  | Zneg p -> Zneg (Coq_Pos.div2_up p)

  (** val log2 : z -> z **)

  let log2 = function
  | Zpos p0 -> (match p0 with
                | XI p -> Zpos (Coq_Pos.size p)
                | XO p -> Zpos (Coq_Pos.size p)
                | XH -> Z0)
  | _ -> Z0

  (** val sqrtrem : z -> z * z **)

  let sqrtrem = function
  | Zpos p ->
    let (s, m) = Coq_Pos.sqrtrem p in
    (match m with
     | Coq_Pos.IsPos r -> ((Zpos s), (Zpos r))
     | _ -> ((Zpos s), Z0))
  | _ -> (Z0, Z0)

  (** val shiftl : z -> z -> z **)

  let shiftl a = function
  | Z0 -> a
  | Zpos p -> Coq_Pos.iter (mul (Zpos (XO XH))) a p
  | Zneg p -> Coq_Pos.iter div2 a p

  (** val eq_dec : z -> z -> bool **)

  let eq_dec x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos x0 -> (match y with
                  | Zpos p0 -> Coq_Pos.eq_dec x0 p0
                  | _ -> false)
    | Zneg x0 -> (match y with
                  | Zneg p0 -> Coq_Pos.eq_dec x0 p0
                  | _ -> false)

  (** val log2_up : z -> z **)

  let log2_up a =
    match compare (Zpos XH) a with
    | Lt -> succ (log2 (pred a))
    | _ -> Z0
 end

(** val z_lt_dec : z -> z -> bool **)

let z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val z_le_dec : z -> z -> bool **)

let z_le_dec x y =
  match Z.compare x y with
  | Gt -> false
  | _ -> true

(** val z_le_gt_dec : z -> z -> bool **)

let z_le_gt_dec =
  z_le_dec

(** val zeq_bool : z -> z -> bool **)

let zeq_bool x y =
  match Z.compare x y with
  | Eq -> true
  | _ -> false

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

let rec substring n0 m s =
  match n0 with
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

let rec index n0 s1 s2 = match s2 with
| [] -> (match n0 with
         | O -> (match s1 with
                 | [] -> Some O
                 | _::_ -> None)
         | S _ -> None)
| _::s2' ->
  (match n0 with
   | O -> if prefix s1 s2 then Some O else (match index O s1 s2' with
                                            | Some n1 -> Some (S n1)
                                            | None -> None)
   | S n' -> (match index n' s1 s2' with
              | Some n1 -> Some (S n1)
              | None -> None))

(** val shift_nat : nat -> positive -> positive **)

let rec shift_nat n0 z0 =
  match n0 with
  | O -> z0
  | S n1 -> XO (shift_nat n1 z0)

(** val shift_pos : positive -> positive -> positive **)

let shift_pos n0 z0 =
  Coq_Pos.iter (fun x -> XO x) z0 n0

(** val two_power_nat : nat -> z **)

let two_power_nat n0 =
  Zpos (shift_nat n0 XH)

(** val zeq : z -> z -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : z -> z -> bool **)

let zlt =
  z_lt_dec

(** val zle : z -> z -> bool **)

let zle =
  z_le_gt_dec

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None

(** val zeven : z -> bool **)

let zeven = function
| Z0 -> true
| Zpos p -> (match p with
             | XO _ -> true
             | _ -> false)
| Zneg p -> (match p with
             | XO _ -> true
             | _ -> false)

type radix = z
  (* singleton inductive, whose constructor was Build_radix *)

(** val radix_val : radix -> z **)

let radix_val r =
  r

(** val radix2 : radix **)

let radix2 =
  Zpos (XO XH)

(** val cond_Zopp : bool -> z -> z **)

let cond_Zopp b m =
  if b then Z.opp m else m

(** val zpos_div_eucl_aux1 : positive -> positive -> z * z **)

let rec zpos_div_eucl_aux1 a b = match b with
| XI _ -> Z.pos_div_eucl a (Zpos b)
| XO b' ->
  (match a with
   | XI a' -> let (q, r) = zpos_div_eucl_aux1 a' b' in (q, (Z.add (Z.mul (Zpos (XO XH)) r) (Zpos XH)))
   | XO a' -> let (q, r) = zpos_div_eucl_aux1 a' b' in (q, (Z.mul (Zpos (XO XH)) r))
   | XH -> (Z0, (Zpos a)))
| XH -> ((Zpos a), Z0)

(** val zpos_div_eucl_aux : positive -> positive -> z * z **)

let zpos_div_eucl_aux a b =
  match Coq_Pos.compare a b with
  | Eq -> ((Zpos XH), Z0)
  | Lt -> (Z0, (Zpos a))
  | Gt -> zpos_div_eucl_aux1 a b

(** val zfast_div_eucl : z -> z -> z * z **)

let zfast_div_eucl a b =
  match a with
  | Z0 -> (Z0, Z0)
  | Zpos a' ->
    (match b with
     | Z0 -> (Z0, Z0)
     | Zpos b' -> zpos_div_eucl_aux a' b'
     | Zneg b' ->
       let (q, r) = zpos_div_eucl_aux a' b' in
       (match r with
        | Z0 -> ((Z.opp q), Z0)
        | _ -> ((Z.opp (Z.add q (Zpos XH))), (Z.add b r))))
  | Zneg a' ->
    (match b with
     | Z0 -> (Z0, Z0)
     | Zpos b' ->
       let (q, r) = zpos_div_eucl_aux a' b' in
       (match r with
        | Z0 -> ((Z.opp q), Z0)
        | _ -> ((Z.opp (Z.add q (Zpos XH))), (Z.sub b r)))
     | Zneg b' -> let (q, r) = zpos_div_eucl_aux a' b' in (q, (Z.opp r)))

(** val iter_nat : ('a1 -> 'a1) -> nat -> 'a1 -> 'a1 **)

let rec iter_nat f n0 x =
  match n0 with
  | O -> x
  | S n' -> iter_nat f n' (f x)

(** val iter_pos : ('a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

let rec iter_pos f n0 x =
  match n0 with
  | XI n' -> iter_pos f n' (iter_pos f n' (f x))
  | XO n' -> iter_pos f n' (iter_pos f n' x)
  | XH -> f x

(** val fLT_exp : z -> z -> z -> z **)

let fLT_exp emin prec e =
  Z.max (Z.sub e prec) emin

(** val digits2_pos : positive -> positive **)

let rec digits2_pos = function
| XI p -> Coq_Pos.succ (digits2_pos p)
| XO p -> Coq_Pos.succ (digits2_pos p)
| XH -> XH

(** val zdigits2 : z -> z **)

let zdigits2 n0 = match n0 with
| Z0 -> n0
| Zpos p -> Zpos (digits2_pos p)
| Zneg p -> Zpos (digits2_pos p)

type location =
| Loc_Exact
| Loc_Inexact of comparison

(** val new_location_even : z -> z -> location -> location **)

let new_location_even nb_steps k l =
  if zeq_bool k Z0
  then (match l with
        | Loc_Exact -> l
        | Loc_Inexact _ -> Loc_Inexact Lt)
  else Loc_Inexact
         (match Z.compare (Z.mul (Zpos (XO XH)) k) nb_steps with
          | Eq -> (match l with
                   | Loc_Exact -> Eq
                   | Loc_Inexact _ -> Gt)
          | x -> x)

(** val new_location_odd : z -> z -> location -> location **)

let new_location_odd nb_steps k l =
  if zeq_bool k Z0
  then (match l with
        | Loc_Exact -> l
        | Loc_Inexact _ -> Loc_Inexact Lt)
  else Loc_Inexact
         (match Z.compare (Z.add (Z.mul (Zpos (XO XH)) k) (Zpos XH)) nb_steps with
          | Eq -> (match l with
                   | Loc_Exact -> Lt
                   | Loc_Inexact l0 -> l0)
          | x -> x)

(** val new_location : z -> z -> location -> location **)

let new_location nb_steps =
  if zeven nb_steps then new_location_even nb_steps else new_location_odd nb_steps

(** val cond_incr : bool -> z -> z **)

let cond_incr b m =
  if b then Z.add m (Zpos XH) else m

(** val round_sign_DN : bool -> location -> bool **)

let round_sign_DN s = function
| Loc_Exact -> false
| Loc_Inexact _ -> s

(** val round_sign_UP : bool -> location -> bool **)

let round_sign_UP s = function
| Loc_Exact -> false
| Loc_Inexact _ -> negb s

(** val round_N : bool -> location -> bool **)

let round_N p = function
| Loc_Exact -> false
| Loc_Inexact c -> (match c with
                    | Eq -> p
                    | Lt -> false
                    | Gt -> true)

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * positive
| F754_finite of bool * positive * z

type nan_pl = positive

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * nan_pl
| B754_finite of bool * positive * z

(** val fF2B : z -> z -> full_float -> binary_float **)

let fF2B _ _ = function
| F754_zero s -> B754_zero s
| F754_infinity s -> B754_infinity s
| F754_nan (b, pl) -> B754_nan (b, pl)
| F754_finite (s, m, e) -> B754_finite (s, m, e)

(** val bopp : z -> z -> (bool -> nan_pl -> bool * nan_pl) -> binary_float -> binary_float **)

let bopp _ _ opp_nan = function
| B754_zero sx -> B754_zero (negb sx)
| B754_infinity sx -> B754_infinity (negb sx)
| B754_nan (sx, plx) -> let (sres, plres) = opp_nan sx plx in B754_nan (sres, plres)
| B754_finite (sx, mx, ex) -> B754_finite ((negb sx), mx, ex)

(** val babs : z -> z -> (bool -> nan_pl -> bool * nan_pl) -> binary_float -> binary_float **)

let babs _ _ abs_nan = function
| B754_zero _ -> B754_zero false
| B754_infinity _ -> B754_infinity false
| B754_nan (sx, plx) -> let (sres, plres) = abs_nan sx plx in B754_nan (sres, plres)
| B754_finite (_, mx, ex) -> B754_finite (false, mx, ex)

(** val bcompare : z -> z -> binary_float -> binary_float -> comparison option **)

let bcompare _ _ f1 f2 =
  match f1 with
  | B754_zero _ ->
    (match f2 with
     | B754_zero _ -> Some Eq
     | B754_infinity b -> if b then Some Gt else Some Lt
     | B754_nan (_, _) -> None
     | B754_finite (b, _, _) -> if b then Some Gt else Some Lt)
  | B754_infinity b ->
    if b
    then (match f2 with
          | B754_infinity b0 -> if b0 then Some Eq else Some Lt
          | B754_nan (_, _) -> None
          | _ -> Some Lt)
    else (match f2 with
          | B754_infinity b0 -> if b0 then Some Gt else Some Eq
          | B754_nan (_, _) -> None
          | _ -> Some Gt)
  | B754_nan (_, _) -> None
  | B754_finite (s1, m1, e1) ->
    if s1
    then (match f2 with
          | B754_zero _ -> Some Lt
          | B754_infinity b -> if b then Some Gt else Some Lt
          | B754_nan (_, _) -> None
          | B754_finite (s2, m2, e2) ->
            if s1
            then if s2
                 then (match Z.compare e1 e2 with
                       | Eq -> Some (compOpp (Coq_Pos.compare_cont Eq m1 m2))
                       | Lt -> Some Gt
                       | Gt -> Some Lt)
                 else Some Lt
            else if s2
                 then Some Gt
                 else (match Z.compare e1 e2 with
                       | Eq -> Some (Coq_Pos.compare_cont Eq m1 m2)
                       | x -> Some x))
    else (match f2 with
          | B754_zero _ -> Some Gt
          | B754_infinity b -> if b then Some Gt else Some Lt
          | B754_nan (_, _) -> None
          | B754_finite (s2, m2, e2) ->
            if s1
            then if s2
                 then (match Z.compare e1 e2 with
                       | Eq -> Some (compOpp (Coq_Pos.compare_cont Eq m1 m2))
                       | Lt -> Some Gt
                       | Gt -> Some Lt)
                 else Some Lt
            else if s2
                 then Some Gt
                 else (match Z.compare e1 e2 with
                       | Eq -> Some (Coq_Pos.compare_cont Eq m1 m2)
                       | x -> Some x))

type shr_record = { shr_m : z; shr_r : bool; shr_s : bool }

(** val shr_m : shr_record -> z **)

let shr_m x = x.shr_m

(** val shr_1 : shr_record -> shr_record **)

let shr_1 mrs =
  let { shr_m = m; shr_r = r; shr_s = s } = mrs in
  let s0 = (||) r s in
  (match m with
   | Z0 -> { shr_m = Z0; shr_r = false; shr_s = s0 }
   | Zpos p0 ->
     (match p0 with
      | XI p -> { shr_m = (Zpos p); shr_r = true; shr_s = s0 }
      | XO p -> { shr_m = (Zpos p); shr_r = false; shr_s = s0 }
      | XH -> { shr_m = Z0; shr_r = true; shr_s = s0 })
   | Zneg p0 ->
     (match p0 with
      | XI p -> { shr_m = (Zneg p); shr_r = true; shr_s = s0 }
      | XO p -> { shr_m = (Zneg p); shr_r = false; shr_s = s0 }
      | XH -> { shr_m = Z0; shr_r = true; shr_s = s0 }))

(** val loc_of_shr_record : shr_record -> location **)

let loc_of_shr_record mrs =
  let { shr_m = _; shr_r = shr_r0; shr_s = shr_s0 } = mrs in
  if shr_r0
  then if shr_s0 then Loc_Inexact Gt else Loc_Inexact Eq
  else if shr_s0 then Loc_Inexact Lt else Loc_Exact

(** val shr_record_of_loc : z -> location -> shr_record **)

let shr_record_of_loc m = function
| Loc_Exact -> { shr_m = m; shr_r = false; shr_s = false }
| Loc_Inexact c ->
  (match c with
   | Eq -> { shr_m = m; shr_r = true; shr_s = false }
   | Lt -> { shr_m = m; shr_r = false; shr_s = true }
   | Gt -> { shr_m = m; shr_r = true; shr_s = true })

(** val shr : shr_record -> z -> z -> shr_record * z **)

let shr mrs e n0 = match n0 with
| Zpos p -> ((iter_pos shr_1 p mrs), (Z.add e n0))
| _ -> (mrs, e)

(** val shr_fexp : z -> z -> z -> z -> location -> shr_record * z **)

let shr_fexp prec emax =
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  let fexp = fLT_exp emin prec in
  (fun m e l -> shr (shr_record_of_loc m l) e (Z.sub (fexp (Z.add (zdigits2 m) e)) e))

type mode =
| Mode_NE
| Mode_ZR
| Mode_DN
| Mode_UP
| Mode_NA

(** val choice_mode : mode -> bool -> z -> location -> z **)

let choice_mode m sx mx lx =
  match m with
  | Mode_NE -> cond_incr (round_N (negb (zeven mx)) lx) mx
  | Mode_ZR -> mx
  | Mode_DN -> cond_incr (round_sign_DN sx lx) mx
  | Mode_UP -> cond_incr (round_sign_UP sx lx) mx
  | Mode_NA -> cond_incr (round_N true lx) mx

(** val overflow_to_inf : mode -> bool -> bool **)

let overflow_to_inf m s =
  match m with
  | Mode_ZR -> false
  | Mode_DN -> s
  | Mode_UP -> negb s
  | _ -> true

(** val binary_overflow : z -> z -> mode -> bool -> full_float **)

let binary_overflow prec emax m s =
  if overflow_to_inf m s
  then F754_infinity s
  else F754_finite (s, (match Z.sub (Z.pow (Zpos (XO XH)) prec) (Zpos XH) with
                        | Zpos p -> p
                        | _ -> XH), (Z.sub emax prec))

(** val binary_round_aux : z -> z -> mode -> bool -> positive -> z -> location -> full_float **)

let binary_round_aux prec emax mode0 sx mx ex lx =
  let (mrs', e') = shr_fexp prec emax (Zpos mx) ex lx in
  let (mrs'', e'') =
    shr_fexp prec emax (choice_mode mode0 sx mrs'.shr_m (loc_of_shr_record mrs')) e' Loc_Exact
  in
  (match mrs''.shr_m with
   | Z0 -> F754_zero sx
   | Zpos m ->
     if Z.leb e'' (Z.sub emax prec) then F754_finite (sx, m, e'') else binary_overflow prec emax mode0 sx
   | Zneg _ -> F754_nan (false, XH))

(** val bmult :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
    binary_float **)

let bmult prec emax mult_nan m x y =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero sx ->
     (match y with
      | B754_zero sy -> B754_zero (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
      | _ -> f (mult_nan x y))
   | B754_infinity sx ->
     (match y with
      | B754_infinity sy -> B754_infinity (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
      | _ -> f (mult_nan x y))
   | B754_nan (_, _) -> f (mult_nan x y)
   | B754_finite (sx, mx, ex) ->
     (match y with
      | B754_zero sy -> B754_zero (xorb sx sy)
      | B754_infinity sy -> B754_infinity (xorb sx sy)
      | B754_nan (_, _) -> f (mult_nan x y)
      | B754_finite (sy, my, ey) ->
        fF2B prec emax (binary_round_aux prec emax m (xorb sx sy) (Coq_Pos.mul mx my) (Z.add ex ey) Loc_Exact)))

(** val shl_align : positive -> z -> z -> positive * z **)

let shl_align mx ex ex' =
  match Z.sub ex' ex with
  | Zneg d -> ((shift_pos d mx), ex')
  | _ -> (mx, ex)

(** val shl_align_fexp : z -> z -> positive -> z -> positive * z **)

let shl_align_fexp prec emax =
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  let fexp = fLT_exp emin prec in (fun mx ex -> shl_align mx ex (fexp (Z.add (Zpos (digits2_pos mx)) ex)))

(** val binary_round : z -> z -> mode -> bool -> positive -> z -> full_float **)

let binary_round prec emax m sx mx ex =
  let (mz, ez) = shl_align_fexp prec emax mx ex in binary_round_aux prec emax m sx mz ez Loc_Exact

(** val binary_normalize : z -> z -> mode -> z -> z -> bool -> binary_float **)

let binary_normalize prec emax mode0 m e szero =
  match m with
  | Z0 -> B754_zero szero
  | Zpos m0 -> fF2B prec emax (binary_round prec emax mode0 false m0 e)
  | Zneg m0 -> fF2B prec emax (binary_round prec emax mode0 true m0 e)

(** val bplus :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
    binary_float **)

let bplus prec emax plus_nan m x y =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero sx ->
     (match y with
      | B754_zero sy ->
        if eqb sx sy then x else (match m with
                                  | Mode_DN -> B754_zero true
                                  | _ -> B754_zero false)
      | B754_nan (_, _) -> f (plus_nan x y)
      | _ -> y)
   | B754_infinity sx ->
     (match y with
      | B754_infinity sy -> if eqb sx sy then x else f (plus_nan x y)
      | B754_nan (_, _) -> f (plus_nan x y)
      | _ -> x)
   | B754_nan (_, _) -> f (plus_nan x y)
   | B754_finite (sx, mx, ex) ->
     (match y with
      | B754_zero _ -> x
      | B754_infinity _ -> y
      | B754_nan (_, _) -> f (plus_nan x y)
      | B754_finite (sy, my, ey) ->
        let ez = Z.min ex ey in
        binary_normalize prec emax m
          (Z.add (cond_Zopp sx (Zpos (fst (shl_align mx ex ez))))
            (cond_Zopp sy (Zpos (fst (shl_align my ey ez))))) ez (match m with
                                                                  | Mode_DN -> true
                                                                  | _ -> false)))

(** val bminus :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
    binary_float **)

let bminus prec emax minus_nan m x y =
  bplus prec emax minus_nan m x (bopp prec emax (fun x0 x1 -> (x0, x1)) y)

(** val fdiv_core_binary : z -> z -> z -> z -> z -> (z * z) * location **)

let fdiv_core_binary prec m1 e1 m2 e2 =
  let d1 = zdigits2 m1 in
  let d2 = zdigits2 m2 in
  let e = Z.sub e1 e2 in
  (match Z.sub (Z.add d2 prec) d1 with
   | Zpos p ->
     let m = Z.shiftl m1 (Zpos p) in
     let e' = Z.add e (Zneg p) in let (q, r) = zfast_div_eucl m m2 in ((q, e'), (new_location m2 r Loc_Exact))
   | _ -> let (q, r) = zfast_div_eucl m1 m2 in ((q, e), (new_location m2 r Loc_Exact)))

(** val bdiv :
    z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
    binary_float **)

let bdiv prec emax div_nan m x y =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero sx ->
     (match y with
      | B754_infinity sy -> B754_zero (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
      | _ -> f (div_nan x y))
   | B754_infinity sx ->
     (match y with
      | B754_zero sy -> B754_infinity (xorb sx sy)
      | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
      | _ -> f (div_nan x y))
   | B754_nan (_, _) -> f (div_nan x y)
   | B754_finite (sx, mx, ex) ->
     (match y with
      | B754_zero sy -> B754_infinity (xorb sx sy)
      | B754_infinity sy -> B754_zero (xorb sx sy)
      | B754_nan (_, _) -> f (div_nan x y)
      | B754_finite (sy, my, ey) ->
        fF2B prec emax
          (let (p, lz) = fdiv_core_binary prec (Zpos mx) ex (Zpos my) ey in
           let (mz, ez) = p in
           (match mz with
            | Zpos mz0 -> binary_round_aux prec emax m (xorb sx sy) mz0 ez lz
            | _ -> F754_nan (false, XH)))))

(** val fsqrt_core_binary : z -> z -> z -> (z * z) * location **)

let fsqrt_core_binary prec m e =
  let d = zdigits2 m in
  let s = Z.max (Z.sub (Z.mul (Zpos (XO XH)) prec) d) Z0 in
  let e' = Z.sub e s in
  if zeven e'
  then let m' = match s with
                | Zpos p -> Z.shiftl m (Zpos p)
                | _ -> m in
       let (q, r) = Z.sqrtrem m' in
       let l = if zeq_bool r Z0 then Loc_Exact else Loc_Inexact (if Z.leb r q then Lt else Gt) in
       ((q, (Z.div2 e')), l)
  else let s' = Z.add s (Zpos XH) in
       let e'' = Z.sub e' (Zpos XH) in
       let m' = match s' with
                | Zpos p -> Z.shiftl m (Zpos p)
                | _ -> m in
       let (q, r) = Z.sqrtrem m' in
       let l = if zeq_bool r Z0 then Loc_Exact else Loc_Inexact (if Z.leb r q then Lt else Gt) in
       ((q, (Z.div2 e'')), l)

(** val bsqrt : z -> z -> (binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float **)

let bsqrt prec emax sqrt_nan m x =
  let f = fun pl -> B754_nan ((fst pl), (snd pl)) in
  (match x with
   | B754_zero _ -> x
   | B754_infinity b -> if b then f (sqrt_nan x) else x
   | B754_nan (_, _) -> f (sqrt_nan x)
   | B754_finite (sx, mx, ex) ->
     if sx
     then f (sqrt_nan x)
     else fF2B prec emax
            (let (p, lz) = fsqrt_core_binary prec (Zpos mx) ex in
             let (mz, ez) = p in
             (match mz with
              | Zpos mz0 -> binary_round_aux prec emax m false mz0 ez lz
              | _ -> F754_nan (false, XH))))

type binary64 = binary_float

(** val default_nan_pl64 : bool * nan_pl **)

let default_nan_pl64 =
  (false,
    (iter_nat (fun x -> XO x) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O))))))))))))))))))))))))))))))))))))))))))))))))))) XH))

(** val unop_nan_pl64 : binary64 -> bool * nan_pl **)

let unop_nan_pl64 = function
| B754_nan (s, pl) -> (s, pl)
| _ -> default_nan_pl64

(** val b64_sqrt : mode -> binary_float -> binary_float **)

let b64_sqrt =
  bsqrt (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
    unop_nan_pl64

(** val default_pl_64 : bool * nan_pl **)

let default_pl_64 =
  (true,
    (let rec f = function
     | O -> XH
     | S n1 -> XO (f n1)
     in f (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          O)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val choose_binop_pl_64 : bool -> nan_pl -> bool -> nan_pl -> bool **)

let choose_binop_pl_64 _ _ _ _ =
  false

type comparison0 =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module Wordsize_64 =
 struct
  (** val wordsize : nat **)

  let wordsize =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 end

module Int64 =
 struct
  (** val wordsize : nat **)

  let wordsize =
    Wordsize_64.wordsize

  (** val modulus : z **)

  let modulus =
    two_power_nat wordsize

  (** val half_modulus : z **)

  let half_modulus =
    Z.div modulus (Zpos (XO XH))

  (** val max_signed : z **)

  let max_signed =
    Z.sub half_modulus (Zpos XH)

  (** val min_signed : z **)

  let min_signed =
    Z.opp half_modulus

  type int = z
    (* singleton inductive, whose constructor was mkint *)

  (** val intval : int -> z **)

  let intval i =
    i

  (** val coq_P_mod_two_p : positive -> nat -> z **)

  let rec coq_P_mod_two_p p = function
  | O -> Z0
  | S m ->
    (match p with
     | XI q -> Z.succ_double (coq_P_mod_two_p q m)
     | XO q -> Z.double (coq_P_mod_two_p q m)
     | XH -> Zpos XH)

  (** val coq_Z_mod_modulus : z -> z **)

  let coq_Z_mod_modulus = function
  | Z0 -> Z0
  | Zpos p -> coq_P_mod_two_p p wordsize
  | Zneg p -> let r = coq_P_mod_two_p p wordsize in if zeq r Z0 then Z0 else Z.sub modulus r

  (** val unsigned : int -> z **)

  let unsigned =
    intval

  (** val signed : int -> z **)

  let signed n0 =
    let x = unsigned n0 in if zlt x half_modulus then x else Z.sub x modulus

  (** val repr : z -> int **)

  let repr =
    coq_Z_mod_modulus
 end

(** val bofZ : z -> z -> z -> binary_float **)

let bofZ prec emax n0 =
  binary_normalize prec emax Mode_NE n0 Z0 false

(** val zofB : z -> z -> binary_float -> z option **)

let zofB _ _ = function
| B754_zero _ -> Some Z0
| B754_finite (s, m, e0) ->
  (match e0 with
   | Z0 -> Some (cond_Zopp s (Zpos m))
   | Zpos e -> Some (Z.mul (cond_Zopp s (Zpos m)) (Z.pow_pos (radix_val radix2) e))
   | Zneg e -> Some (cond_Zopp s (Z.div (Zpos m) (Z.pow_pos (radix_val radix2) e))))
| _ -> None

(** val zofB_range : z -> z -> binary_float -> z -> z -> z option **)

let zofB_range prec emax f zmin zmax =
  match zofB prec emax f with
  | Some z0 -> if (&&) (Z.leb zmin z0) (Z.leb z0 zmax) then Some z0 else None
  | None -> None

(** val pos_pow : positive -> positive -> positive **)

let rec pos_pow x = function
| XI y0 -> Coq_Pos.mul x (Coq_Pos.square (pos_pow x y0))
| XO y0 -> Coq_Pos.square (pos_pow x y0)
| XH -> x

(** val bparse : z -> z -> positive -> positive -> z -> binary_float **)

let bparse prec emax base m e =
  let emin = Z.sub (Z.sub (Zpos (XI XH)) emax) prec in
  (match e with
   | Z0 -> bofZ prec emax (Zpos m)
   | Zpos p ->
     if Z.ltb (Z.mul e (Z.log2 (Zpos base))) emax
     then bofZ prec emax (Z.mul (Zpos m) (Zpos (pos_pow base p)))
     else B754_infinity false
   | Zneg p ->
     if Z.ltb (Z.add (Z.mul e (Z.log2 (Zpos base))) (Z.log2_up (Zpos m))) emin
     then B754_zero false
     else fF2B prec emax
            (let (p0, lz) = fdiv_core_binary prec (Zpos m) Z0 (Zpos (pos_pow base p)) Z0 in
             let (mz, ez) = p0 in
             (match mz with
              | Zpos mz0 -> binary_round_aux prec emax Mode_NE (xorb false false) mz0 ez lz
              | _ -> F754_nan (false, XH))))

type float = binary64

(** val cmp_of_comparison : comparison0 -> comparison option -> bool **)

let cmp_of_comparison c x =
  match c with
  | Ceq -> (match x with
            | Some c0 -> (match c0 with
                          | Eq -> true
                          | _ -> false)
            | None -> false)
  | Cne -> (match x with
            | Some c0 -> (match c0 with
                          | Eq -> false
                          | _ -> true)
            | None -> true)
  | Clt -> (match x with
            | Some c0 -> (match c0 with
                          | Lt -> true
                          | _ -> false)
            | None -> false)
  | Cle -> (match x with
            | Some c0 -> (match c0 with
                          | Gt -> false
                          | _ -> true)
            | None -> false)
  | Cgt -> (match x with
            | Some c0 -> (match c0 with
                          | Gt -> true
                          | _ -> false)
            | None -> false)
  | Cge -> (match x with
            | Some c0 -> (match c0 with
                          | Lt -> false
                          | _ -> true)
            | None -> false)

module Float =
 struct
  (** val transform_quiet_pl : nan_pl -> nan_pl **)

  let transform_quiet_pl pl =
    Coq_Pos.coq_lor pl
      (iter_nat (fun x -> XO x) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O))))))))))))))))))))))))))))))))))))))))))))))))))) XH)

  (** val neg_pl : bool -> nan_pl -> bool * nan_pl **)

  let neg_pl s pl =
    ((negb s), pl)

  (** val abs_pl : bool -> nan_pl -> bool * nan_pl **)

  let abs_pl _ pl =
    (false, pl)

  (** val binop_pl : binary64 -> binary64 -> bool * nan_pl **)

  let binop_pl x y =
    match x with
    | B754_nan (s1, pl1) ->
      (match y with
       | B754_nan (s2, pl2) ->
         if choose_binop_pl_64 s1 pl1 s2 pl2
         then (s2, (transform_quiet_pl pl2))
         else (s1, (transform_quiet_pl pl1))
       | _ -> (s1, (transform_quiet_pl pl1)))
    | _ -> (match y with
            | B754_nan (s2, pl2) -> (s2, (transform_quiet_pl pl2))
            | _ -> default_pl_64)

  (** val zero : float **)

  let zero =
    B754_zero false

  (** val neg : float -> float **)

  let neg =
    bopp (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) neg_pl

  (** val abs : float -> float **)

  let abs =
    babs (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) abs_pl

  (** val add : float -> float -> float **)

  let add =
    bplus (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
      binop_pl Mode_NE

  (** val sub : float -> float -> float **)

  let sub =
    bminus (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
      binop_pl Mode_NE

  (** val mul : float -> float -> float **)

  let mul =
    bmult (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
      binop_pl Mode_NE

  (** val div : float -> float -> float **)

  let div =
    bdiv (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
      binop_pl Mode_NE

  (** val compare : float -> float -> comparison option **)

  let compare f1 f2 =
    bcompare (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
      f1 f2

  (** val cmp : comparison0 -> float -> float -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (compare f1 f2)

  (** val to_long : float -> Int64.int option **)

  let to_long f =
    option_map Int64.repr
      (zofB_range (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
        XH))))))))))) f Int64.min_signed Int64.max_signed)

  (** val from_parsed : positive -> positive -> z -> float **)

  let from_parsed base intPart expPart =
    bparse (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH)))))))))))
      base intPart expPart
 end

type 'm monadOps = { bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm); ret : (__ -> __ -> 'm) }

(** val bind : 'a1 monadOps -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monadOps0 x x0 =
  let { bind = bind0; ret = _ } = monadOps0 in Obj.magic bind0 __ __ x x0

(** val ret : 'a1 monadOps -> 'a2 -> 'a1 **)

let ret monadOps0 x =
  let { bind = _; ret = ret0 } = monadOps0 in Obj.magic ret0 __ x

type 't monadTransformerOps = { tops : (__ -> __ monadOps -> 't monadOps);
                                lift : (__ -> __ monadOps -> __ -> __ -> 't) }

(** val tops : 'a1 monadTransformerOps -> 'a2 monadOps -> 'a1 monadOps **)

let tops monadTransformerOps0 h =
  let { tops = tops0; lift = _ } = monadTransformerOps0 in Obj.magic tops0 __ h

(** val idM : __ monadOps **)

let idM =
  { bind = (fun _ _ x f -> f x); ret = (fun _ x -> x) }

(** val monad_of_transformer : 'a1 monadTransformerOps -> 'a1 monadOps **)

let monad_of_transformer h =
  tops h idM

(** val optionT : __ monadTransformerOps **)

let optionT =
  { tops = (fun _ h -> { bind = (fun _ _ x f ->
    bind h x (fun a -> match a with
                       | Some a' -> f a'
                       | None -> ret h None)); ret = (fun _ x -> ret h (Some x)) }); lift = (fun _ h _ x ->
    bind h x (fun a -> ret h (Some a))) }

(** val optionM : __ option monadOps **)

let optionM =
  monad_of_transformer (Obj.magic optionT)

module FloatIO =
 struct
  (** val coq_Z_to_float : z -> float **)

  let coq_Z_to_float = function
  | Z0 -> Float.zero
  | Zpos x -> Float.from_parsed (XO (XI (XO XH))) x Z0
  | Zneg x -> Float.neg (Float.from_parsed (XO (XI (XO XH))) x Z0)

  module Details =
   struct
    (** val precision_of_float : z **)

    let precision_of_float =
      Zpos (XI (XO (XI (XO (XI XH)))))

    (** val log10of2scaled3 : z **)

    let log10of2scaled3 =
      Zpos (XI (XO (XI (XI (XO (XI (XO (XO XH))))))))

    (** val strToUintHelper : char list -> uint option **)

    let rec strToUintHelper = function
    | [] -> Some Nil
    | x::x0 ->
      bind (Obj.magic optionM) (strToUintHelper x0) (fun lower ->
        (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
          (fun b b0 b1 b2 b3 b4 b5 b6 ->
          if b
          then if b0
               then if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D7 lower)
                                   else None
                              else None
                    else if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D3 lower)
                                   else None
                              else None
               else if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D5 lower)
                                   else None
                              else None
                    else if b2
                         then if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D9 lower)
                                   else None
                              else None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D1 lower)
                                   else None
                              else None
          else if b0
               then if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D6 lower)
                                   else None
                              else None
                    else if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D2 lower)
                                   else None
                              else None
               else if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D4 lower)
                                   else None
                              else None
                    else if b2
                         then if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D8 lower)
                                   else None
                              else None
                         else if b3
                              then if b4
                                   then if b5 then None else if b6 then None else Some (D0 lower)
                                   else None
                              else None)
          x)

    (** val strToUint : char list -> uint option **)

    let strToUint s =
      bind (Obj.magic optionM) (strToUintHelper s) (fun x -> Some (unorm x))

    (** val strToIntHelper : char list -> int option **)

    let strToIntHelper s = match s with
    | [] -> Some (Pos (D0 Nil))
    | x::x0 ->
      (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
        (fun b b0 b1 b2 b3 b4 b5 b6 ->
        if b
        then if b0
             then if b1
                  then let sign = false in
                       bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                         (if sign then Neg uintVal else Pos uintVal))
                  else if b2
                       then if b3
                            then let sign = false in
                                 bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                                   (if sign then Neg uintVal else Pos uintVal))
                            else if b4
                                 then if b5
                                      then let sign = false in
                                           bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal ->
                                             Some (if sign then Neg uintVal else Pos uintVal))
                                      else if b6
                                           then let sign = false in
                                                bind (Obj.magic optionM) (Obj.magic strToUint s)
                                                  (fun uintVal -> Some
                                                  (if sign then Neg uintVal else Pos uintVal))
                                           else let sign = false in
                                                bind (Obj.magic optionM) (Obj.magic strToUint x0)
                                                  (fun uintVal -> Some
                                                  (if sign then Neg uintVal else Pos uintVal))
                                 else let sign = false in
                                      bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                                        (if sign then Neg uintVal else Pos uintVal))
                       else let sign = false in
                            bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                              (if sign then Neg uintVal else Pos uintVal))
             else if b1
                  then if b2
                       then if b3
                            then let sign = false in
                                 bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                                   (if sign then Neg uintVal else Pos uintVal))
                            else if b4
                                 then if b5
                                      then let sign = false in
                                           bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal ->
                                             Some (if sign then Neg uintVal else Pos uintVal))
                                      else if b6
                                           then let sign = false in
                                                bind (Obj.magic optionM) (Obj.magic strToUint s)
                                                  (fun uintVal -> Some
                                                  (if sign then Neg uintVal else Pos uintVal))
                                           else let sign = true in
                                                bind (Obj.magic optionM) (Obj.magic strToUint x0)
                                                  (fun uintVal -> Some
                                                  (if sign then Neg uintVal else Pos uintVal))
                                 else let sign = false in
                                      bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                                        (if sign then Neg uintVal else Pos uintVal))
                       else let sign = false in
                            bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                              (if sign then Neg uintVal else Pos uintVal))
                  else let sign = false in
                       bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
                         (if sign then Neg uintVal else Pos uintVal))
        else let sign = false in
             bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal -> Some
               (if sign then Neg uintVal else Pos uintVal)))
        x

    (** val strToInt : char list -> int option **)

    let strToInt s =
      bind (Obj.magic optionM) (strToIntHelper s) (fun x -> Some (norm x))

    (** val strToZ : char list -> z option **)

    let strToZ s =
      bind (Obj.magic optionM) (Obj.magic strToInt s) (fun x -> Some (Z.of_int x))

    (** val splitAtExponent : char list -> char list * char list **)

    let splitAtExponent s =
      let maybeEpos = match index O ('E'::[]) s with
                      | Some epos -> Some epos
                      | None -> index O ('e'::[]) s in
      (match maybeEpos with
       | Some epos -> ((substring O epos s), (substring (S epos) (sub (sub (length s) epos) (S O)) s))
       | None -> (s, []))

    (** val splitAtPoint : char list -> char list * char list **)

    let splitAtPoint s =
      match index O ('.'::[]) s with
      | Some dpos -> ((substring O dpos s), (substring (S dpos) (sub (sub (length s) dpos) (S O)) s))
      | None -> (s, [])

    (** val decomposeFloatString : char list -> (char list * char list) * char list **)

    let decomposeFloatString s =
      let (frac, exp) = splitAtExponent s in ((splitAtPoint frac), exp)

    (** val strToFloatHelper : char list -> (z * z) option **)

    let strToFloatHelper s =
      let (p, exp) = decomposeFloatString s in
      let (intPart, fracPart) = p in
      let mantstring = append intPart fracPart in
      bind (Obj.magic optionM) (Obj.magic strToZ mantstring) (fun zmant ->
        bind (Obj.magic optionM) (Obj.magic strToZ exp) (fun zexp ->
          let adjustedZexp = Z.sub zexp (Z.of_nat (length fracPart)) in
          ret (Obj.magic optionM) (zmant, adjustedZexp)))

    (** val strToFloat : char list -> float option **)

    let strToFloat s =
      match strToFloatHelper s with
      | Some p ->
        let (zmant, adjustedZexp) = p in
        ret (Obj.magic optionM)
          (match zmant with
           | Z0 -> Float.zero
           | Zpos x -> Float.from_parsed (XO (XI (XO XH))) x adjustedZexp
           | Zneg x -> Float.neg (Float.from_parsed (XO (XI (XO XH))) x adjustedZexp))
      | None -> None

    (** val scale_exp : z -> z -> z **)

    let scale_exp d e =
      Z.sub (Z.sub d (Zpos (XO XH)))
        (Z.div (Z.mul (Z.sub (Z.add e precision_of_float) (Zpos XH)) log10of2scaled3) (Zpos (XO (XO (XO (XI
          (XO (XI (XI (XI (XI XH)))))))))))

    (** val digits : z -> char list **)

    let digits = function
    | Z0 -> '0'::[]
    | Zpos p ->
      (match p with
       | XI p0 ->
         (match p0 with
          | XI p1 ->
            (match p1 with
             | XI p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 'v'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XH -> 'n'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XH -> 'f'::[])
             | XO p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 'r'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | XO p4 ->
                     (match p4 with
                      | XH -> 'z'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | XH -> 'j'::[])
                | XH -> 'b'::[])
             | XH -> '7'::[])
          | XO p1 ->
            (match p1 with
             | XI p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 't'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XH -> 'l'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XH -> 'd'::[])
             | XO p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 'p'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | XO p4 ->
                     (match p4 with
                      | XH -> 'x'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | XH -> 'h'::[])
                | XH -> '9'::[])
             | XH -> '5'::[])
          | XH -> '3'::[])
       | XO p0 ->
         (match p0 with
          | XI p1 ->
            (match p1 with
             | XI p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 'u'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XH -> 'm'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XH -> 'e'::[])
             | XO p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 'q'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | XO p4 ->
                     (match p4 with
                      | XH -> 'y'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | XH -> 'i'::[])
                | XH -> 'a'::[])
             | XH -> '6'::[])
          | XO p1 ->
            (match p1 with
             | XI p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 's'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XH -> 'k'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XH -> 'c'::[])
             | XO p2 ->
               (match p2 with
                | XI p3 ->
                  (match p3 with
                   | XH -> 'o'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | XO p3 ->
                  (match p3 with
                   | XI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | XO p4 ->
                     (match p4 with
                      | XH -> 'w'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | XH -> 'g'::[])
                | XH -> '8'::[])
             | XH -> '4'::[])
          | XH -> '2'::[])
       | XH -> '1'::[])
    | Zneg _ ->
      '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))

    (** val repeat_string : nat -> char list -> char list **)

    let rec repeat_string n0 c =
      match n0 with
      | O -> []
      | S x -> append c (repeat_string x c)

    (** val coq_Z_to_string_base10_aux : nat -> nat -> z -> char list **)

    let rec coq_Z_to_string_base10_aux min_digits fuel num =
      if Z.ltb num Z0
      then 'R'::('e'::('q'::('u'::('i'::('r'::('e'::(' '::('n'::('o'::('n'::('-'::('n'::('e'::('g'::('a'::('t'::('i'::('v'::('e'::(' '::('n'::('u'::('m'::('.'::[]))))))))))))))))))))))))
      else (match fuel with
            | O -> 'o'::('u'::('t'::(' '::('o'::('f'::(' '::('f'::('u'::('e'::('l'::[]))))))))))
            | S x ->
              (match num with
               | Z0 -> repeat_string min_digits ('0'::[])
               | _ ->
                 append (coq_Z_to_string_base10_aux (pred min_digits) x (Z.div num (Zpos (XO (XI (XO XH))))))
                   (digits (Z.modulo num (Zpos (XO (XI (XO XH))))))))

    (** val coq_Z_to_string_base10 : nat -> z -> char list **)

    let coq_Z_to_string_base10 min_digits num =
      if Z.ltb num Z0
      then append ('-'::[])
             (coq_Z_to_string_base10_aux min_digits (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
               (Z.opp num))
      else if Z.eqb num Z0
           then repeat_string min_digits ('0'::[])
           else coq_Z_to_string_base10_aux min_digits (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                  num

    (** val scaled_float_to_Z : float -> z -> z **)

    let scaled_float_to_Z x fdigs0 =
      match Float.to_long (Float.mul x (Float.from_parsed (XO (XI (XO XH))) XH fdigs0)) with
      | Some ii -> Int64.signed ii
      | None -> Z0

    (** val insert_decimal : char list -> nat -> char list **)

    let insert_decimal s fdigs0 =
      let len = length s in
      let front = sub len fdigs0 in append (substring O front s) (append ('.'::[]) (substring front fdigs0 s))

    (** val float_to_string_unsigned : float -> nat -> char list **)

    let float_to_string_unsigned x fdigs0 =
      match x with
      | B754_finite (b, _, e) ->
        if b
        then []
        else let digs_after_dec = pred fdigs0 in
             let scale = scale_exp (Z.of_nat fdigs0) e in
             let scaled = scaled_float_to_Z x scale in
             let b10 = coq_Z_to_string_base10 (S O) scaled in
             let lb10 = length b10 in
             let scale' = if Nat.ltb lb10 fdigs0 then Z.add scale (Z.of_nat (sub fdigs0 lb10)) else scale in
             let b10' =
               if Nat.ltb lb10 fdigs0
               then let scaled' = scaled_float_to_Z x scale' in coq_Z_to_string_base10 fdigs0 scaled'
               else b10
             in
             let d10 = insert_decimal b10' digs_after_dec in
             let true_exp = Z.sub (Z.sub (Z.of_nat fdigs0) scale') (Zpos XH) in
             let exp_string = coq_Z_to_string_base10 (S O) true_exp in
             append d10 (append ('e'::[]) exp_string)
      | _ -> []

    (** val float_to_string_unpadded : float -> nat -> char list **)

    let float_to_string_unpadded x fdigs0 =
      match x with
      | B754_zero b -> if b then '-'::('0'::('.'::('0'::[]))) else '0'::('.'::('0'::[]))
      | B754_infinity b -> if b then '-'::('i'::('n'::('f'::[]))) else 'i'::('n'::('f'::[]))
      | B754_nan (b, _) -> if b then '-'::('n'::('a'::('n'::[]))) else 'n'::('a'::('n'::[]))
      | B754_finite (b, _, _) ->
        if b
        then append ('-'::[]) (float_to_string_unsigned (Float.abs x) fdigs0)
        else float_to_string_unsigned x fdigs0

    (** val pad_to_width : nat -> char list -> char list **)

    let pad_to_width width0 s =
      let ls = length s in let pads = repeat_string (sub width0 ls) (' '::[]) in append pads s

    (** val float_to_string : nat -> nat -> float -> char list **)

    let float_to_string width0 fdigs0 x =
      pad_to_width width0 (float_to_string_unpadded x fdigs0)
   end

  (** val coq_Z_to_string_base10 : nat -> z -> char list **)

  let coq_Z_to_string_base10 =
    Details.coq_Z_to_string_base10

  (** val float_to_string : nat -> nat -> float -> char list **)

  let float_to_string =
    Details.float_to_string

  (** val strToFloat : char list -> float option **)

  let strToFloat =
    Details.strToFloat

  (** val strToFloat' : char list -> float **)

  let strToFloat' s =
    match strToFloat s with
    | Some x -> x
    | None -> Float.zero
 end

module DebugIO =
 struct
  (** val width : nat **)

  let width =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S O)))))))))))))

  (** val fdigs : nat **)

  let fdigs =
    S (S (S (S (S (S (S (S (S O))))))))

  (** val print_float : float -> char list **)

  let print_float =
    FloatIO.float_to_string width fdigs

  (** val print_Z : z -> char list **)

  let print_Z =
    FloatIO.coq_Z_to_string_base10 (S O)
 end

module PTree =
 struct
  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  (** val empty : 'a1 t **)

  let empty =
    Leaf

  (** val get : positive -> 'a1 t -> 'a1 option **)

  let rec get i = function
  | Leaf -> None
  | Node (l, o, r) -> (match i with
                       | XI ii -> get ii r
                       | XO ii -> get ii l
                       | XH -> o)

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec set i v = function
  | Leaf ->
    (match i with
     | XI ii -> Node (Leaf, None, (set ii v Leaf))
     | XO ii -> Node ((set ii v Leaf), None, Leaf)
     | XH -> Node (Leaf, (Some v), Leaf))
  | Node (l, o, r) ->
    (match i with
     | XI ii -> Node (l, o, (set ii v r))
     | XO ii -> Node ((set ii v l), o, r)
     | XH -> Node (l, (Some v), r))

  (** val remove : positive -> 'a1 t -> 'a1 t **)

  let rec remove i m =
    match i with
    | XI ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match l with
          | Leaf ->
            (match o with
             | Some _ -> Node (l, o, (remove ii r))
             | None ->
               (match remove ii r with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node (Leaf, None, (Node (t0, o0, t1)))))
          | Node (_, _, _) -> Node (l, o, (remove ii r))))
    | XO ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match o with
          | Some _ -> Node ((remove ii l), o, r)
          | None ->
            (match r with
             | Leaf ->
               (match remove ii l with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node ((Node (t0, o0, t1)), None, Leaf))
             | Node (_, _, _) -> Node ((remove ii l), o, r))))
    | XH ->
      (match m with
       | Leaf -> Leaf
       | Node (l, _, r) ->
         (match l with
          | Leaf -> (match r with
                     | Leaf -> Leaf
                     | Node (_, _, _) -> Node (l, None, r))
          | Node (_, _, _) -> Node (l, None, r)))

  (** val bempty : 'a1 t -> bool **)

  let rec bempty = function
  | Leaf -> true
  | Node (l, o, r) -> (match o with
                       | Some _ -> false
                       | None -> (&&) (bempty l) (bempty r))

  (** val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let rec beq beqA m1 m2 =
    match m1 with
    | Leaf -> bempty m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> bempty m1
       | Node (l2, o2, r2) ->
         (&&)
           ((&&)
             (match o1 with
              | Some y1 -> (match o2 with
                            | Some y2 -> beqA y1 y2
                            | None -> false)
              | None -> (match o2 with
                         | Some _ -> false
                         | None -> true)) (beq beqA l1 l2)) (beq beqA r1 r2))

  (** val prev_append : positive -> positive -> positive **)

  let rec prev_append i j =
    match i with
    | XI i' -> prev_append i' (XI j)
    | XO i' -> prev_append i' (XO j)
    | XH -> j

  (** val prev : positive -> positive **)

  let prev i =
    prev_append i XH

  (** val coq_Node' : 'a1 t -> 'a1 option -> 'a1 t -> 'a1 t **)

  let coq_Node' l x r =
    match l with
    | Leaf ->
      (match x with
       | Some _ -> Node (l, x, r)
       | None -> (match r with
                  | Leaf -> Leaf
                  | Node (_, _, _) -> Node (l, x, r)))
    | Node (_, _, _) -> Node (l, x, r)

  (** val xcombine_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t **)

  let rec xcombine_l f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> coq_Node' (xcombine_l f l) (f o None) (xcombine_l f r)

  (** val xcombine_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t **)

  let rec xcombine_r f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> coq_Node' (xcombine_r f l) (f None o) (xcombine_r f r)

  (** val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let rec combine f m1 m2 =
    match m1 with
    | Leaf -> xcombine_r f m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> xcombine_l f m1
       | Node (l2, o2, r2) -> coq_Node' (combine f l1 l2) (f o1 o2) (combine f r1 r2))

  (** val xelements : 'a1 t -> positive -> (positive * 'a1) list -> (positive * 'a1) list **)

  let rec xelements m i k =
    match m with
    | Leaf -> k
    | Node (l, o, r) ->
      (match o with
       | Some x -> xelements l (XO i) (((prev i), x) :: (xelements r (XI i) k))
       | None -> xelements l (XO i) (xelements r (XI i) k))

  (** val elements : 'a1 t -> (positive * 'a1) list **)

  let elements m =
    xelements m XH []
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module ITree =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PTree.t

  (** val empty : 'a1 t **)

  let empty =
    PTree.empty

  (** val get : elt -> 'a1 t -> 'a1 option **)

  let get k m =
    PTree.get (X.index k) m

  (** val set : elt -> 'a1 -> 'a1 t -> 'a1 t **)

  let set k v m =
    PTree.set (X.index k) v m

  (** val remove : elt -> 'a1 t -> 'a1 t **)

  let remove k m =
    PTree.remove (X.index k) m

  (** val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let beq =
    PTree.beq

  (** val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let combine =
    PTree.combine
 end

type stateVar =
| SvT
| SvX
| SvXD
| SvXDD
| SvCOEFF_OF_REST
| SvGRAVITY
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT

(** val state_var_eq : stateVar -> stateVar -> bool **)

let state_var_eq r1 r2 =
  match r1 with
  | SvT -> (match r2 with
            | SvT -> true
            | _ -> false)
  | SvX -> (match r2 with
            | SvX -> true
            | _ -> false)
  | SvXD -> (match r2 with
             | SvXD -> true
             | _ -> false)
  | SvXDD -> (match r2 with
              | SvXDD -> true
              | _ -> false)
  | SvCOEFF_OF_REST -> (match r2 with
                        | SvCOEFF_OF_REST -> true
                        | _ -> false)
  | SvGRAVITY -> (match r2 with
                  | SvGRAVITY -> true
                  | _ -> false)
  | SvT_STOP -> (match r2 with
                 | SvT_STOP -> true
                 | _ -> false)
  | SvDT -> (match r2 with
             | SvDT -> true
             | _ -> false)
  | SvDT_MAX -> (match r2 with
                 | SvDT_MAX -> true
                 | _ -> false)
  | SvDT_MIN -> (match r2 with
                 | SvDT_MIN -> true
                 | _ -> false)
  | SvDT_PRINT -> (match r2 with
                   | SvDT_PRINT -> true
                   | _ -> false)

(** val svIndex : stateVar -> positive **)

let svIndex = function
| SvT -> XH
| SvX -> XO XH
| SvXD -> XI XH
| SvXDD -> XO (XO XH)
| SvCOEFF_OF_REST -> XI (XO XH)
| SvGRAVITY -> XO (XI XH)
| SvT_STOP -> XI (XI XH)
| SvDT -> XO (XO (XO XH))
| SvDT_MAX -> XI (XO (XO XH))
| SvDT_MIN -> XO (XI (XO XH))
| SvDT_PRINT -> XI (XI (XO XH))

(** val svStrList : (stateVar * char list) list **)

let svStrList =
  (SvT, ('T'::[])) :: ((SvX, ('X'::[])) :: ((SvXD, ('X'::('D'::[]))) :: ((SvXDD,
    ('X'::('D'::('D'::[])))) :: ((SvCOEFF_OF_REST,
    ('C'::('O'::('E'::('F'::('F'::('_'::('O'::('F'::('_'::('R'::('E'::('S'::('T'::[])))))))))))))) :: ((SvGRAVITY,
    ('G'::('R'::('A'::('V'::('I'::('T'::('Y'::[])))))))) :: ((SvT_STOP,
    ('T'::('_'::('S'::('T'::('O'::('P'::[]))))))) :: ((SvDT, ('D'::('T'::[]))) :: ((SvDT_MAX,
    ('D'::('T'::('_'::('M'::('A'::('X'::[]))))))) :: ((SvDT_MIN,
    ('D'::('T'::('_'::('M'::('I'::('N'::[]))))))) :: ((SvDT_PRINT,
    ('D'::('T'::('_'::('P'::('R'::('I'::('N'::('T'::[]))))))))) :: []))))))))))

module SvIndex =
 struct
  type t = stateVar

  (** val index : stateVar -> positive **)

  let index =
    svIndex

  (** val eq : stateVar -> stateVar -> bool **)

  let eq =
    state_var_eq
 end

module SvTree = ITree(SvIndex)

type stringSvTreeTy = char list SvTree.t

(** val emptyStringPTree : char list SvTree.t **)

let emptyStringPTree =
  SvTree.empty

(** val updateStringSvTree : (stateVar * char list) list -> stringSvTreeTy -> stringSvTreeTy **)

let rec updateStringSvTree valList intree =
  match valList with
  | [] -> intree
  | p :: vltail ->
    let (sv, val0) = p in let tree1 = SvTree.set sv val0 intree in updateStringSvTree vltail tree1

(** val svToStringTree : stringSvTreeTy **)

let svToStringTree =
  updateStringSvTree svStrList emptyStringPTree

(** val svToStrOpt : stateVar -> char list option **)

let svToStrOpt sv =
  SvTree.get sv svToStringTree

(** val svToStr : stateVar -> char list **)

let svToStr sv =
  match svToStrOpt sv with
  | Some x -> x
  | None -> []

type floatSvTreeTy = float SvTree.t

(** val emptyFloatPTree : float SvTree.t **)

let emptyFloatPTree =
  SvTree.empty

(** val updateFloatSvTree : (stateVar * float) list -> floatSvTreeTy -> floatSvTreeTy **)

let rec updateFloatSvTree valList intree =
  match valList with
  | [] -> intree
  | p :: vltail ->
    let (sv, fval) = p in let tree1 = SvTree.set sv fval intree in updateFloatSvTree vltail tree1

(** val stringKeyValToFloatKeyVal : (stateVar * char list) list -> (stateVar * float) list **)

let rec stringKeyValToFloatKeyVal = function
| [] -> []
| p :: tl ->
  let (sv, str) = p in
  let tailkvs = stringKeyValToFloatKeyVal tl in
  (match FloatIO.strToFloat str with
   | Some fval -> (sv, fval) :: tailkvs
   | None -> tailkvs)

(** val driver_defaults_str : (stateVar * char list) list **)

let driver_defaults_str =
  (SvT, ('0'::('.'::('0'::[])))) :: ((SvT_STOP, ('0'::('.'::('0'::[])))) :: ((SvDT,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MAX,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MIN,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_PRINT, ('0'::('.'::('0'::('1'::[]))))) :: [])))))

(** val driver_defaults : unit -> (stateVar * float) list **)

let driver_defaults _ =
  stringKeyValToFloatKeyVal driver_defaults_str

(** val model_default_values_str : (stateVar * char list) list **)

let model_default_values_str =
  (SvT, ('0'::('.'::('0'::[])))) :: ((SvX, ('1'::('0'::('.'::('0'::[]))))) :: ((SvXD,
    ('0'::('.'::('0'::[])))) :: ((SvXDD, ('0'::('.'::('0'::[])))) :: ((SvCOEFF_OF_REST,
    ('0'::('.'::('8'::('0'::[]))))) :: ((SvGRAVITY, ('9'::('.'::('8'::('8'::[]))))) :: ((SvT_STOP,
    ('1'::('0'::('.'::('0'::[]))))) :: ((SvDT, ('0'::('.'::('0'::('1'::[]))))) :: ((SvDT_MAX,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MIN,
    ('0'::('.'::('0'::('0'::('1'::[])))))) :: ((SvDT_PRINT, ('0'::('.'::('0'::('1'::[]))))) :: []))))))))))

(** val model_default_values : unit -> (stateVar * float) list **)

let model_default_values _ =
  stringKeyValToFloatKeyVal model_default_values_str

(** val state0 : unit -> floatSvTreeTy **)

let state0 _ =
  let driverState = updateFloatSvTree (driver_defaults ()) emptyFloatPTree in
  updateFloatSvTree (model_default_values ()) driverState

(** val svGetFloat : stateVar -> floatSvTreeTy -> float **)

let svGetFloat sv tree0 =
  match SvTree.get sv tree0 with
  | Some x -> x
  | None -> Float.zero

(** val modelOutputs : stateVar list **)

let modelOutputs =
  SvT :: (SvX :: (SvXD :: []))

type flagsTy = { stop_simulation : bool; end_of_run : bool; evaluate_xd : bool }

(** val stop_simulation : flagsTy -> bool **)

let stop_simulation x = x.stop_simulation

(** val end_of_run : flagsTy -> bool **)

let end_of_run x = x.end_of_run

(** val evaluate_xd : flagsTy -> bool **)

let evaluate_xd x = x.evaluate_xd

type eventFuncId =
| EfiStop
| EfiAppendSolution
| EfiTerminateSim
| EfiFlip_Xd_At_Bounce

type eventTy = { key : char list; time : float; func : eventFuncId }

(** val key : eventTy -> char list **)

let key x = x.key

(** val time : eventTy -> float **)

let time x = x.time

(** val func : eventTy -> eventFuncId **)

let func x = x.func

type logEntryTy = { le_caption : char list; le_vars : (char list * char list) list;
                    le_events : (char list * char list) list }

type simTy = { vars : floatSvTreeTy; solkeys : stateVar list; solution : char list list list;
               sim_events : eventTy list; log : logEntryTy list; flags : flagsTy }

(** val vars : simTy -> floatSvTreeTy **)

let vars x = x.vars

(** val solkeys : simTy -> stateVar list **)

let solkeys x = x.solkeys

(** val solution : simTy -> char list list list **)

let solution x = x.solution

(** val sim_events : simTy -> eventTy list **)

let sim_events x = x.sim_events

(** val log : simTy -> logEntryTy list **)

let log x = x.log

(** val flags : simTy -> flagsTy **)

let flags x = x.flags

(** val log_sim : char list -> simTy -> simTy **)

let log_sim _ sim =
  sim

(** val default_flags : flagsTy **)

let default_flags =
  { stop_simulation = false; end_of_run = false; evaluate_xd = false }

(** val t_ge_tstop_event_func : eventTy -> simTy -> simTy * float option **)

let t_ge_tstop_event_func _ sim =
  let sim1 =
    log_sim
      ('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))
      sim
  in
  let vars0 = sim1.vars in
  let t0 = svGetFloat SvT vars0 in
  let t_stop = svGetFloat SvT_STOP vars0 in
  let stop_sim = Float.cmp Cge t0 t_stop in
  let new_flags =
    set stop_simulation (fun f e -> { stop_simulation = (f e.stop_simulation); end_of_run = e.end_of_run;
      evaluate_xd = e.evaluate_xd }) (constructor stop_sim) sim1.flags
  in
  let result_sim =
    set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events =
      e.sim_events; log = e.log; flags = (f e.flags) }) (constructor new_flags) sim1
  in
  let result_log =
    log_sim
      ('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::('_'::('s'::('i'::('m'::[])))))))))))))))))))))))))))))))))
      result_sim
  in
  (result_log, None)

(** val make_solution_row_helper : floatSvTreeTy -> stateVar list -> char list list **)

let rec make_solution_row_helper vars0 = function
| [] -> []
| key0 :: others ->
  let fval = svGetFloat key0 vars0 in
  let sval = DebugIO.print_float fval in sval :: (make_solution_row_helper vars0 others)

(** val make_solution_row : simTy -> simTy **)

let make_solution_row sim =
  let row = make_solution_row_helper sim.vars sim.solkeys in
  let result_sim =
    set solution (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = (f e.solution); sim_events =
      e.sim_events; log = e.log; flags = e.flags }) (fun old -> row :: old) sim
  in
  log_sim
    ('m'::('a'::('k'::('e'::('_'::('s'::('o'::('l'::('u'::('t'::('i'::('o'::('n'::('_'::('r'::('o'::('w'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))))))))
    result_sim

(** val append_solution_event_func : eventTy -> simTy -> simTy * float option **)

let append_solution_event_func _ sim =
  let sim1 = make_solution_row sim in
  let sim1log =
    log_sim
      ('a'::('p'::('p'::('e'::('n'::('d'::('_'::('s'::('o'::('l'::('u'::('t'::('i'::('o'::('n'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))))))
      sim1
  in
  let vars0 = sim1log.vars in
  let dt_print = svGetFloat SvDT_PRINT vars0 in
  let t0 = svGetFloat SvT vars0 in let new_t = Float.add t0 dt_print in (sim1log, (Some new_t))

(** val terminate_sim_event_func : eventTy -> simTy -> simTy * float option **)

let terminate_sim_event_func _ sim =
  let simlog =
    log_sim
      ('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('s'::('i'::('m'::[])))))))))))))))))))))))))))))
      sim
  in
  (simlog, None)

(** val driver_default_events : eventTy list **)

let driver_default_events =
  { key =
    ('t'::('_'::('g'::('e'::('_'::('t'::('s'::('t'::('o'::('p'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))));
    time = Float.zero; func = EfiStop } :: ({ key =
    ('a'::('p'::('p'::('e'::('n'::('d'::('_'::('l'::('o'::('g'::('_'::('e'::('v'::('e'::('n'::('t'::[]))))))))))))))));
    time = Float.zero; func = EfiAppendSolution } :: ({ key =
    ('t'::('e'::('r'::('m'::('i'::('n'::('a'::('t'::('e'::('_'::('s'::('i'::('m'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))));
    time = Float.zero; func = EfiTerminateSim } :: []))

(** val default_sim : simTy **)

let default_sim =
  { vars = (state0 ()); solkeys = modelOutputs; solution = []; sim_events = driver_default_events; log = [];
    flags = default_flags }

(** val default_sim_log : simTy **)

let default_sim_log =
  log_sim ('d'::('e'::('f'::('a'::('u'::('l'::('t'::('_'::('s'::('i'::('m'::[]))))))))))) default_sim

(** val f99 : float **)

let f99 =
  FloatIO.strToFloat' ('9'::('9'::('.'::('0'::[]))))

(** val bounceEvent : eventTy **)

let bounceEvent =
  { key =
    ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))))))));
    time = f99; func = EfiFlip_Xd_At_Bounce }

(** val init_sim : simTy -> simTy **)

let init_sim sim =
  let result_sim =
    set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events =
      (f e.sim_events); log = e.log; flags = e.flags }) (fun evs -> bounceEvent :: evs) sim
  in
  log_sim
    ('i'::('n'::('i'::('t'::('_'::('s'::('i'::('m'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))))
    result_sim

(** val set_vars : simTy -> floatSvTreeTy -> simTy **)

let set_vars sim new_vars =
  let result_sim =
    set vars (fun f e -> { vars = (f e.vars); solkeys = e.solkeys; solution = e.solution; sim_events =
      e.sim_events; log = e.log; flags = e.flags }) (constructor new_vars) sim
  in
  log_sim
    ('s'::('e'::('t'::('_'::('v'::('a'::('r'::('s'::(':'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))))
    result_sim

(** val flip_xd_at_bounce_event_func : eventTy -> simTy -> simTy * float option **)

let flip_xd_at_bounce_event_func _ sim =
  let sim' =
    log_sim
      ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::(':'::(' '::('s'::('i'::('m'::[])))))))))))))))))))))))))))))))))
      sim
  in
  let vars0 = sim'.vars in
  let coeff_of_rest = svGetFloat SvCOEFF_OF_REST vars0 in
  let xd = svGetFloat SvXD vars0 in
  let new_xd = Float.neg (Float.mul coeff_of_rest xd) in
  let vars' = SvTree.set SvXD new_xd vars0 in
  let new_flags =
    set evaluate_xd (fun f e -> { stop_simulation = e.stop_simulation; end_of_run = e.end_of_run;
      evaluate_xd = (f e.evaluate_xd) }) (constructor true) sim'.flags
  in
  let sim2 = set_vars sim' vars' in
  let result_sim =
    set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events =
      e.sim_events; log = e.log; flags = (f e.flags) }) (constructor new_flags) sim2
  in
  let result_log =
    log_sim
      ('f'::('l'::('i'::('p'::('_'::('x'::('d'::('_'::('a'::('t'::('_'::('b'::('o'::('u'::('n'::('c'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::('_'::('f'::('u'::('n'::('c'::[]))))))))))))))))))))))))))))
      result_sim
  in
  (result_log, (Some f99))

(** val handle_event : eventTy -> simTy -> simTy * float option **)

let handle_event this sim =
  let sim_log =
    log_sim
      ('h'::('a'::('n'::('d'::('l'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::(':'::('s'::('i'::('m'::[]))))))))))))))))
      sim
  in
  let result =
    match this.func with
    | EfiStop -> t_ge_tstop_event_func this sim_log
    | EfiAppendSolution -> append_solution_event_func this sim_log
    | EfiTerminateSim -> terminate_sim_event_func this sim_log
    | EfiFlip_Xd_At_Bounce -> flip_xd_at_bounce_event_func this sim_log
  in
  let (result_sim, maybefloat) = result in
  let result_log =
    log_sim
      ('h'::('a'::('n'::('d'::('l'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))
      result_sim
  in
  (result_log, maybefloat)

(** val set_var : stateVar -> float -> simTy -> simTy **)

let set_var key0 val0 sim =
  let sim_log = log_sim ('s'::('e'::('t'::('_'::('v'::('a'::('r'::(':'::('s'::('i'::('m'::[]))))))))))) sim in
  let result_sim =
    set vars (fun f e -> { vars = (f e.vars); solkeys = e.solkeys; solution = e.solution; sim_events =
      e.sim_events; log = e.log; flags = e.flags }) (constructor (SvTree.set key0 val0 sim.vars)) sim_log
  in
  log_sim
    ('s'::('e'::('t'::('_'::('v'::('a'::('r'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))
    result_sim

(** val fhalf : float **)

let fhalf =
  FloatIO.strToFloat' ('0'::('.'::('5'::[])))

(** val ftwo : float **)

let ftwo =
  FloatIO.strToFloat' ('2'::('.'::('0'::[])))

(** val epsilon : float **)

let epsilon =
  FloatIO.strToFloat' ('0'::('.'::('0'::('0'::('0'::('0'::('0'::('0'::('0'::('0'::('0'::('1'::[]))))))))))))

(** val efi_eqb : eventFuncId -> eventFuncId -> bool **)

let efi_eqb fi1 fi2 =
  match fi1 with
  | EfiStop -> (match fi2 with
                | EfiStop -> true
                | _ -> false)
  | EfiAppendSolution -> (match fi2 with
                          | EfiAppendSolution -> true
                          | _ -> false)
  | EfiTerminateSim -> (match fi2 with
                        | EfiTerminateSim -> true
                        | _ -> false)
  | EfiFlip_Xd_At_Bounce -> (match fi2 with
                             | EfiFlip_Xd_At_Bounce -> true
                             | _ -> false)

(** val schedule_event : eventTy list -> eventFuncId -> float -> eventTy list **)

let rec schedule_event evs efi new_time =
  match evs with
  | [] -> []
  | ev :: evtl ->
    let ev' =
      if efi_eqb ev.func efi
      then set time (fun f e -> { key = e.key; time = (f e.time); func = e.func }) (constructor new_time) ev
      else ev
    in
    ev' :: (schedule_event evtl efi new_time)

(** val sqrt : float -> float **)

let sqrt arg =
  b64_sqrt Mode_NE arg

(** val fone : float **)

let fone =
  FloatIO.strToFloat' ('1'::('.'::('0'::[])))

(** val differential_equations : simTy -> simTy **)

let differential_equations sim =
  let sim_log =
    log_sim
      ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::[]))))))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let t0 = svGetFloat SvT vars0 in
  let x = svGetFloat SvX vars0 in
  let xd = svGetFloat SvXD vars0 in
  let dt_min = svGetFloat SvDT_MIN vars0 in
  let dt_max = svGetFloat SvDT_MAX vars0 in
  let gravity = svGetFloat SvGRAVITY vars0 in
  let t_stop = svGetFloat SvT_STOP vars0 in
  let xdd = Float.neg gravity in
  let sim1 = set_var SvXDD xdd sim_log in
  let sim1log =
    log_sim
      ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))))))))))
      sim1
  in
  let est_max =
    Float.add (Float.add x (Float.mul xd dt_max)) (Float.mul (Float.mul (Float.mul fhalf xdd) dt_max) dt_max)
  in
  let dt_impact = Float.add t_stop fone in
  let dt_impact2 =
    if Float.cmp Cle est_max Float.zero
    then let est_min =
           Float.add (Float.add x (Float.mul xd dt_min))
             (Float.mul (Float.mul (Float.mul fhalf xdd) dt_min) dt_min)
         in
         if Float.cmp Cle est_min Float.zero
         then Float.zero
         else let dt_impact3 =
                Float.div
                  (Float.sub (Float.neg xd)
                    (sqrt (Float.sub (Float.mul xd xd) (Float.mul (Float.mul ftwo x) xdd))))
                  (Float.mul ftwo x)
              in
              if Float.cmp Cgt (Float.sub dt_min dt_impact3) epsilon
              then Float.div
                     (Float.add (Float.neg xd)
                       (sqrt (Float.sub (Float.mul xd xd) (Float.mul (Float.mul ftwo x) xdd))))
                     (Float.mul ftwo x)
              else dt_impact3
    else dt_impact
  in
  let impact_time = Float.add t0 dt_impact2 in
  let events' = schedule_event sim1log.sim_events EfiFlip_Xd_At_Bounce impact_time in
  let result_sim =
    set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events =
      (f e.sim_events); log = e.log; flags = e.flags }) (constructor events') sim1log
  in
  log_sim
    ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::('i'::('a'::('l'::('_'::('e'::('q'::('u'::('a'::('t'::('i'::('o'::('n'::('s'::[]))))))))))))))))))))))
    result_sim

(** val zofFloat : float -> z **)

let zofFloat f =
  match zofB (Zpos (XI (XO (XI (XO (XI XH)))))) (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO XH))))))))))) f with
  | Some z0 -> z0
  | None -> Z0

(** val round : float -> float **)

let round f =
  let z0 = zofFloat f in FloatIO.coq_Z_to_float z0

(** val tenTo6 : float **)

let tenTo6 =
  FloatIO.strToFloat' ('1'::('.'::('e'::('6'::[]))))

(** val process_one_event : eventTy -> simTy -> eventTy * simTy **)

let process_one_event ev sim =
  let sim_log =
    log_sim
      ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('o'::('n'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::(':'::('s'::('i'::('m'::[])))))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let t0 = svGetFloat SvT vars0 in
  let dt_min = svGetFloat SvDT_MIN vars0 in
  let et = ev.time in
  let etdelta = round (Float.mul (Float.sub et t0) tenTo6) in
  let minrnd = round (Float.mul dt_min tenTo6) in
  let (ev', result_sim) =
    if Float.cmp Clt etdelta minrnd
    then let (sim2, new_time_opt) = handle_event ev sim_log in
         let sim2log =
           log_sim
             ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('o'::('n'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::(':'::('s'::('i'::('m'::('2'::[]))))))))))))))))))))))
             sim2
         in
         (match new_time_opt with
          | Some new_time ->
            ((set time (fun f e -> { key = e.key; time = (f e.time); func = e.func }) (constructor new_time)
               ev), sim2log)
          | None -> (ev, sim2log))
    else (ev, sim_log)
  in
  let result_log =
    log_sim
      ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('o'::('n'::('e'::('_'::('e'::('v'::('e'::('n'::('t'::[])))))))))))))))))
      result_sim
  in
  (ev', result_log)

(** val process_events_helper : eventTy list -> simTy -> eventTy list * simTy **)

let rec process_events_helper evs sim =
  match evs with
  | [] -> ([], sim)
  | ev :: evtl ->
    let (ev', sim') = process_one_event ev sim in
    let (evs', sim'') = process_events_helper evtl sim' in ((ev' :: evs'), sim'')

(** val min_event_time : eventTy list -> float -> float **)

let rec min_event_time evs min_so_far =
  match evs with
  | [] -> min_so_far
  | ev :: evtl ->
    let et = ev.time in
    let new_min = if (&&) (Float.cmp Cgt et Float.zero) (Float.cmp Clt et min_so_far) then et else min_so_far
    in
    min_event_time evtl new_min

(** val process_events : simTy -> simTy **)

let process_events sim =
  let sim_log =
    log_sim
      ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('s'::('i'::('m'::[]))))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let events = sim_log.sim_events in
  let result_sim =
    match events with
    | [] -> set_var SvDT (svGetFloat SvDT_MAX vars0) sim_log
    | _ :: _ ->
      let t0 = svGetFloat SvT vars0 in
      let dt_max = svGetFloat SvDT_MAX vars0 in
      let (evs', sim1) = process_events_helper events sim_log in
      let sim1log =
        log_sim
          ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))
          sim1
      in
      let vars' = sim1log.vars in
      let min_time0 = svGetFloat SvT_STOP vars' in
      let min_time = min_event_time evs' min_time0 in
      let time_to_next_event = Float.sub min_time t0 in
      let new_dt =
        if (&&) (Float.cmp Cgt time_to_next_event Float.zero)
             (Float.cmp Cgt (Float.sub dt_max time_to_next_event) epsilon)
        then time_to_next_event
        else dt_max
      in
      let sim3 = set_var SvDT new_dt sim1log in
      let sim4 =
        set sim_events (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events =
          (f e.sim_events); log = e.log; flags = e.flags }) (constructor evs') sim3
      in
      log_sim
        ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('s'::('i'::('m'::('4'::[])))))))))))))))))))
        sim4
  in
  log_sim
    ('p'::('r'::('o'::('c'::('e'::('s'::('s'::('_'::('e'::('v'::('e'::('n'::('t'::('s'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))))
    result_sim

(** val advance_states : (stateVar * stateVar) list -> float -> simTy -> simTy **)

let rec advance_states pairs dt sim =
  let result_sim =
    match pairs with
    | [] -> sim
    | p :: pairstl ->
      let (sv, svd) = p in
      let vars0 = sim.vars in
      let el = svGetFloat sv vars0 in
      let eld = svGetFloat svd vars0 in
      let el' = Float.add el (Float.mul eld dt) in
      let sim1 = set_var sv el' sim in
      let sim1log =
        log_sim
          ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('s'::('t'::('a'::('t'::('e'::('s'::(':'::('s'::('i'::('m'::('1'::[])))))))))))))))))))
          sim1
      in
      advance_states pairstl dt sim1log
  in
  log_sim
    ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('s'::('t'::('a'::('t'::('e'::('s'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))))))))
    result_sim

(** val advance_model : simTy -> simTy **)

let advance_model sim =
  let sim_log =
    log_sim
      ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('m'::('o'::('d'::('e'::('l'::(':'::('s'::('i'::('m'::[])))))))))))))))))
      sim
  in
  let vars0 = sim_log.vars in
  let dt = svGetFloat SvDT vars0 in
  let pairs = (SvX, SvXD) :: ((SvXD, SvXDD) :: []) in
  let sim2 = advance_states pairs dt sim_log in
  let sim2log =
    log_sim
      ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('m'::('o'::('d'::('e'::('l'::(':'::('s'::('i'::('m'::('2'::[]))))))))))))))))))
      sim2
  in
  let vars' = sim2log.vars in
  let t0 = svGetFloat SvT vars' in
  let new_t = Float.add t0 dt in
  let rounded_new_t = Float.div (round (Float.mul new_t tenTo6)) tenTo6 in
  let result_sim = set_var SvT rounded_new_t sim2log in
  log_sim
    ('a'::('d'::('v'::('a'::('n'::('c'::('e'::('_'::('m'::('o'::('d'::('e'::('l'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))))))))
    result_sim

(** val oneStep : simTy -> simTy **)

let oneStep sim =
  let sim_log = log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::[]))))))))))) sim in
  let sim1 = differential_equations sim_log in
  let sim1log =
    log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('1'::[])))))))))))) sim1
  in
  let sim2 = process_events sim1log in
  let sim2log =
    log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('2'::[])))))))))))) sim2
  in
  let sim3 =
    if sim2log.flags.evaluate_xd
    then let sim4 = differential_equations sim2log in
         let sim4log =
           log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('4'::[])))))))))))) sim4
         in
         let flags4 = sim4log.flags in
         let flags4' =
           set evaluate_xd (fun f e -> { stop_simulation = e.stop_simulation; end_of_run = e.end_of_run;
             evaluate_xd = (f e.evaluate_xd) }) (constructor false) flags4
         in
         let sim6 =
           set flags (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events =
             e.sim_events; log = e.log; flags = (f e.flags) }) (constructor flags4') sim4log
         in
         log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('6'::[])))))))))))) sim6
    else sim2log
  in
  let sim3log =
    log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('3'::[])))))))))))) sim3
  in
  let sim5 = advance_model sim3log in
  log_sim ('o'::('n'::('e'::('S'::('t'::('e'::('p'::(':'::('s'::('i'::('m'::('5'::[])))))))))))) sim5

(** val run_sim_loop : z -> simTy -> simTy **)

let rec run_sim_loop steps sim =
  if zle Z0 steps
  then if (||) sim.flags.end_of_run sim.flags.stop_simulation
       then sim
       else run_sim_loop (Z.sub steps (Zpos XH)) (oneStep sim)
  else sim

(** val run_sim : simTy -> simTy **)

let run_sim sim =
  let sim_log = log_sim ('r'::('u'::('n'::('_'::('s'::('i'::('m'::(':'::('s'::('i'::('m'::[]))))))))))) sim in
  let vars0 = sim_log.vars in
  let dtmin = svGetFloat SvDT_MIN vars0 in
  let tstop = svGetFloat SvT_STOP vars0 in
  let max_steps_float = Float.div tstop dtmin in
  let steps = zofFloat max_steps_float in
  let result_sim = run_sim_loop steps sim_log in
  log_sim
    ('r'::('u'::('n'::('_'::('s'::('i'::('m'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))
    result_sim

(** val sim_in : unit -> simTy **)

let sim_in _ =
  let sim1log =
    log_sim ('s'::('i'::('m'::('_'::('i'::('n'::(':'::('s'::('i'::('m'::('1'::[]))))))))))) default_sim_log
  in
  let sim2 = init_sim sim1log in
  let sim2log = log_sim ('s'::('i'::('m'::('_'::('i'::('n'::(':'::('s'::('i'::('m'::('2'::[]))))))))))) sim2
  in
  let sim3 = set_var SvCOEFF_OF_REST (FloatIO.strToFloat' ('0'::('.'::('8'::('8'::[]))))) sim2log in
  let sim3log = log_sim ('s'::('i'::('m'::('_'::('i'::('n'::(':'::('s'::('i'::('m'::('3'::[]))))))))))) sim3
  in
  let sim4 = set_var SvGRAVITY (FloatIO.strToFloat' ('9'::('.'::('8'::('8'::[]))))) sim3log in
  log_sim ('s'::('i'::('m'::('_'::('i'::('n'::(':'::('s'::('i'::('m'::('4'::[]))))))))))) sim4

(** val main : unit -> simTy **)

let main _ =
  let sim1 = sim_in () in
  let sim2 = run_sim sim1 in
  let sim2log =
    log_sim
      ('s'::('i'::('m'::('_'::('m'::('a'::('i'::('n'::(':'::('r'::('e'::('s'::('u'::('l'::('t'::[])))))))))))))))
      sim2
  in
  let sim3 =
    set solution (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = (f e.solution); sim_events =
      e.sim_events; log = e.log; flags = e.flags }) rev sim2log
  in
  set log (fun f e -> { vars = e.vars; solkeys = e.solkeys; solution = e.solution; sim_events = e.sim_events;
    log = (f e.log); flags = e.flags }) rev sim3
