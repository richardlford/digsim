module Coqlib where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified Datatypes
import qualified List0
import qualified Specif
import qualified Wf
import qualified ZArith_dec

__ :: any
__ = Prelude.error "Logical or arity value used"

peq :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
peq =
  BinPos._Pos__eq_dec

plt :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
plt x y =
  let {c = BinPos._Pos__compare x y} in
  case c of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

positive_rec :: a1 -> (Prelude.Integer -> a1 -> a1) -> Prelude.Integer -> a1
positive_rec v1 f =
  let {
   iter = \x p ->
    case peq x 1 of {
     Prelude.True -> v1;
     Prelude.False -> f (BinPos._Pos__pred x) (p (BinPos._Pos__pred x) __)}}
  in
  Wf.coq_Fix iter

zeq :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
zeq =
  BinInt._Z__eq_dec

zlt :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
zlt =
  ZArith_dec.coq_Z_lt_dec

zle :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
zle =
  ZArith_dec.coq_Z_le_gt_dec

coq_Zdivide_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zdivide_dec p q =
  zeq (BinInt._Z__modulo q p) 0

nat_of_Z :: Prelude.Integer -> Prelude.Integer
nat_of_Z =
  BinInt._Z__to_nat

align :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
align n amount =
  (Prelude.*)
    (BinInt._Z__div ((Prelude.-) ((Prelude.+) n amount) ((\x -> x) 1))
      amount) amount

option_eq :: (a1 -> a1 -> Prelude.Bool) -> (Prelude.Maybe a1) ->
             (Prelude.Maybe a1) -> Prelude.Bool
option_eq eqA x y =
  Datatypes.option_rect (\a x0 ->
    case x0 of {
     Prelude.Just a0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False)
        (eqA a a0);
     Prelude.Nothing -> Prelude.False}) (\x0 ->
    case x0 of {
     Prelude.Just _ -> Prelude.False;
     Prelude.Nothing -> Prelude.True}) x y

option_map :: (a1 -> a2) -> (Prelude.Maybe a1) -> Prelude.Maybe a2
option_map f x =
  case x of {
   Prelude.Just y -> Prelude.Just (f y);
   Prelude.Nothing -> Prelude.Nothing}

sum_left_map :: (a1 -> a2) -> (Prelude.Either a1 a3) -> Prelude.Either a2 a3
sum_left_map f x =
  case x of {
   Prelude.Left y -> Prelude.Left (f y);
   Prelude.Right z -> Prelude.Right z}

list_length_z_aux :: (([]) a1) -> Prelude.Integer -> Prelude.Integer
list_length_z_aux l acc =
  case l of {
   ([]) -> acc;
   (:) _ tl -> list_length_z_aux tl (BinInt._Z__succ acc)}

list_length_z :: (([]) a1) -> Prelude.Integer
list_length_z l =
  list_length_z_aux l 0

list_nth_z :: (([]) a1) -> Prelude.Integer -> Prelude.Maybe a1
list_nth_z l n =
  case l of {
   ([]) -> Prelude.Nothing;
   (:) hd tl ->
    case zeq n 0 of {
     Prelude.True -> Prelude.Just hd;
     Prelude.False -> list_nth_z tl (BinInt._Z__pred n)}}

list_fold_left :: (a1 -> a2 -> a2) -> a2 -> (([]) a1) -> a2
list_fold_left f accu l =
  case l of {
   ([]) -> accu;
   (:) x l' -> list_fold_left f (f x accu) l'}

list_fold_right :: (a1 -> a2 -> a2) -> (([]) a1) -> a2 -> a2
list_fold_right f l base =
  list_fold_left f base (List0.rev' l)

list_disjoint_dec :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) 
                     a1) -> Prelude.Bool
list_disjoint_dec eqA_dec l1 =
  Datatypes.list_rect (\_ -> Prelude.True) (\a _ iHl1 l2 ->
    case List0.in_dec eqA_dec a l2 of {
     Prelude.True -> Prelude.False;
     Prelude.False -> iHl1 l2}) l1

list_norepet_dec :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
list_norepet_dec eqA_dec l =
  Datatypes.list_rec Prelude.True (\a l0 iHl ->
    case iHl of {
     Prelude.True ->
      case List0.in_dec eqA_dec a l0 of {
       Prelude.True -> Prelude.False;
       Prelude.False -> Prelude.True};
     Prelude.False -> Prelude.False}) l

list_drop :: Prelude.Integer -> (([]) a1) -> ([]) a1
list_drop n x =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> x)
    (\n' -> case x of {
             ([]) -> ([]);
             (:) _ tl -> list_drop n' tl})
    n

list_repeat :: Prelude.Integer -> a1 -> ([]) a1
list_repeat n x =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\m -> (:) x (list_repeat m x))
    n

data Coq_nlist a =
   Coq_nbase a
 | Coq_ncons a (Coq_nlist a)

nlist_rect :: (a1 -> a2) -> (a1 -> (Coq_nlist a1) -> a2 -> a2) -> (Coq_nlist
              a1) -> a2
nlist_rect f f0 n =
  case n of {
   Coq_nbase y -> f y;
   Coq_ncons y n0 -> f0 y n0 (nlist_rect f f0 n0)}

nlist_rec :: (a1 -> a2) -> (a1 -> (Coq_nlist a1) -> a2 -> a2) -> (Coq_nlist
             a1) -> a2
nlist_rec =
  nlist_rect

nfirst :: (Coq_nlist a1) -> a1
nfirst l =
  case l of {
   Coq_nbase a -> a;
   Coq_ncons a _ -> a}

nlast :: (Coq_nlist a1) -> a1
nlast l =
  case l of {
   Coq_nbase a -> a;
   Coq_ncons _ l' -> nlast l'}

