open BinNums

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
                       | Coq_xI ii -> get ii r
                       | Coq_xO ii -> get ii l
                       | Coq_xH -> o)

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec set i v = function
  | Leaf ->
    (match i with
     | Coq_xI ii -> Node (Leaf, None, (set ii v Leaf))
     | Coq_xO ii -> Node ((set ii v Leaf), None, Leaf)
     | Coq_xH -> Node (Leaf, (Some v), Leaf))
  | Node (l, o, r) ->
    (match i with
     | Coq_xI ii -> Node (l, o, (set ii v r))
     | Coq_xO ii -> Node ((set ii v l), o, r)
     | Coq_xH -> Node (l, (Some v), r))

  (** val remove : positive -> 'a1 t -> 'a1 t **)

  let rec remove i m =
    match i with
    | Coq_xI ii ->
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
    | Coq_xO ii ->
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
    | Coq_xH ->
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
    | Coq_xI i' -> prev_append i' (Coq_xI j)
    | Coq_xO i' -> prev_append i' (Coq_xO j)
    | Coq_xH -> j

  (** val prev : positive -> positive **)

  let prev i =
    prev_append i Coq_xH

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
       | Some x -> xelements l (Coq_xO i) (((prev i), x) :: (xelements r (Coq_xI i) k))
       | None -> xelements l (Coq_xO i) (xelements r (Coq_xI i) k))

  (** val elements : 'a1 t -> (positive * 'a1) list **)

  let elements m =
    xelements m Coq_xH []
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
