open BinNums

module PTree :
 sig
  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  val empty : 'a1 t

  val get : positive -> 'a1 t -> 'a1 option

  val set : positive -> 'a1 -> 'a1 t -> 'a1 t

  val remove : positive -> 'a1 t -> 'a1 t

  val bempty : 'a1 t -> bool

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val prev_append : positive -> positive -> positive

  val prev : positive -> positive

  val coq_Node' : 'a1 t -> 'a1 option -> 'a1 t -> 'a1 t

  val xcombine_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

  val xcombine_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

  val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val xelements : 'a1 t -> positive -> (positive * 'a1) list -> (positive * 'a1) list

  val elements : 'a1 t -> (positive * 'a1) list
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module ITree :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
 end
