module BinPos where

import qualified Prelude
import qualified BinNums
import qualified Bool
import qualified Datatypes
import qualified Decimal
import qualified Logic
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

type Pos__Coq_t = Prelude.Integer

_Pos__succ :: Prelude.Integer -> Prelude.Integer
_Pos__succ x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> (\x -> 2 Prelude.* x) (_Pos__succ p))
    (\p -> (\x -> 2 Prelude.* x Prelude.+ 1) p)
    (\_ -> (\x -> 2 Prelude.* x) 1)
    x

_Pos__add :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__add x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x) (_Pos__add_carry p q))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__add p q))
      (\_ -> (\x -> 2 Prelude.* x) (_Pos__succ p))
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__add p q))
      (\q -> (\x -> 2 Prelude.* x) (_Pos__add p q))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) p)
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x) (_Pos__succ q))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) q)
      (\_ -> (\x -> 2 Prelude.* x) 1)
      y)
    x

_Pos__add_carry :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__add_carry x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__add_carry p q))
      (\q -> (\x -> 2 Prelude.* x) (_Pos__add_carry p q))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__succ p))
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x) (_Pos__add_carry p q))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__add p q))
      (\_ -> (\x -> 2 Prelude.* x) (_Pos__succ p))
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__succ q))
      (\q -> (\x -> 2 Prelude.* x) (_Pos__succ q))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) 1)
      y)
    x

_Pos__pred_double :: Prelude.Integer -> Prelude.Integer
_Pos__pred_double x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) p))
    (\p -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__pred_double p))
    (\_ -> 1)
    x

_Pos__pred :: Prelude.Integer -> Prelude.Integer
_Pos__pred x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> (\x -> 2 Prelude.* x) p)
    (\p -> _Pos__pred_double p)
    (\_ -> 1)
    x

_Pos__pred_N :: Prelude.Integer -> BinNums.N
_Pos__pred_N x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> BinNums.Npos ((\x -> 2 Prelude.* x) p))
    (\p -> BinNums.Npos (_Pos__pred_double p))
    (\_ -> BinNums.N0)
    x

data Pos__Coq_mask =
   Pos__IsNul
 | Pos__IsPos Prelude.Integer
 | Pos__IsNeg

_Pos__mask_rect :: a1 -> (Prelude.Integer -> a1) -> a1 -> Pos__Coq_mask -> a1
_Pos__mask_rect f f0 f1 m =
  case m of {
   Pos__IsNul -> f;
   Pos__IsPos x -> f0 x;
   Pos__IsNeg -> f1}

_Pos__mask_rec :: a1 -> (Prelude.Integer -> a1) -> a1 -> Pos__Coq_mask -> a1
_Pos__mask_rec =
  _Pos__mask_rect

_Pos__succ_double_mask :: Pos__Coq_mask -> Pos__Coq_mask
_Pos__succ_double_mask x =
  case x of {
   Pos__IsNul -> Pos__IsPos 1;
   Pos__IsPos p -> Pos__IsPos ((\x -> 2 Prelude.* x Prelude.+ 1) p);
   Pos__IsNeg -> Pos__IsNeg}

_Pos__double_mask :: Pos__Coq_mask -> Pos__Coq_mask
_Pos__double_mask x =
  case x of {
   Pos__IsPos p -> Pos__IsPos ((\x -> 2 Prelude.* x) p);
   x0 -> x0}

_Pos__double_pred_mask :: Prelude.Integer -> Pos__Coq_mask
_Pos__double_pred_mask x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> Pos__IsPos ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    p)))
    (\p -> Pos__IsPos ((\x -> 2 Prelude.* x) (_Pos__pred_double p)))
    (\_ -> Pos__IsNul)
    x

_Pos__pred_mask :: Pos__Coq_mask -> Pos__Coq_mask
_Pos__pred_mask p =
  case p of {
   Pos__IsPos q ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Pos__IsPos (_Pos__pred q))
      (\_ -> Pos__IsPos (_Pos__pred q))
      (\_ -> Pos__IsNul)
      q;
   _ -> Pos__IsNeg}

_Pos__sub_mask :: Prelude.Integer -> Prelude.Integer -> Pos__Coq_mask
_Pos__sub_mask x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__double_mask (_Pos__sub_mask p q))
      (\q -> _Pos__succ_double_mask (_Pos__sub_mask p q))
      (\_ -> Pos__IsPos ((\x -> 2 Prelude.* x) p))
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__succ_double_mask (_Pos__sub_mask_carry p q))
      (\q -> _Pos__double_mask (_Pos__sub_mask p q))
      (\_ -> Pos__IsPos (_Pos__pred_double p))
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Pos__IsNeg)
      (\_ -> Pos__IsNeg)
      (\_ -> Pos__IsNul)
      y)
    x

_Pos__sub_mask_carry :: Prelude.Integer -> Prelude.Integer -> Pos__Coq_mask
_Pos__sub_mask_carry x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__succ_double_mask (_Pos__sub_mask_carry p q))
      (\q -> _Pos__double_mask (_Pos__sub_mask p q))
      (\_ -> Pos__IsPos (_Pos__pred_double p))
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__double_mask (_Pos__sub_mask_carry p q))
      (\q -> _Pos__succ_double_mask (_Pos__sub_mask_carry p q))
      (\_ -> _Pos__double_pred_mask p)
      y)
    (\_ -> Pos__IsNeg)
    x

_Pos__sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__sub x y =
  case _Pos__sub_mask x y of {
   Pos__IsPos z -> z;
   _ -> 1}

_Pos__mul :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__mul x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> _Pos__add y ((\x -> 2 Prelude.* x) (_Pos__mul p y)))
    (\p -> (\x -> 2 Prelude.* x) (_Pos__mul p y))
    (\_ -> y)
    x

_Pos__iter :: (a1 -> a1) -> a1 -> Prelude.Integer -> a1
_Pos__iter f x n =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\n' -> f (_Pos__iter f (_Pos__iter f x n') n'))
    (\n' -> _Pos__iter f (_Pos__iter f x n') n')
    (\_ -> f x)
    n

_Pos__pow :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__pow x =
  _Pos__iter (_Pos__mul x) 1

_Pos__square :: Prelude.Integer -> Prelude.Integer
_Pos__square p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    (_Pos__add (_Pos__square p0) p0)))
    (\p0 -> (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    (_Pos__square p0)))
    (\_ -> 1)
    p

_Pos__div2 :: Prelude.Integer -> Prelude.Integer
_Pos__div2 p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> p0)
    (\p0 -> p0)
    (\_ -> 1)
    p

_Pos__div2_up :: Prelude.Integer -> Prelude.Integer
_Pos__div2_up p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> _Pos__succ p0)
    (\p0 -> p0)
    (\_ -> 1)
    p

_Pos__size_nat :: Prelude.Integer -> Prelude.Integer
_Pos__size_nat p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> Prelude.succ (_Pos__size_nat p0))
    (\p0 -> Prelude.succ (_Pos__size_nat p0))
    (\_ -> Prelude.succ 0)
    p

_Pos__size :: Prelude.Integer -> Prelude.Integer
_Pos__size p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> _Pos__succ (_Pos__size p0))
    (\p0 -> _Pos__succ (_Pos__size p0))
    (\_ -> 1)
    p

_Pos__compare_cont :: Datatypes.Coq_comparison -> Prelude.Integer ->
                      Prelude.Integer -> Datatypes.Coq_comparison
_Pos__compare_cont r x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__compare_cont r p q)
      (\q -> _Pos__compare_cont Datatypes.Gt p q)
      (\_ -> Datatypes.Gt)
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Pos__compare_cont Datatypes.Lt p q)
      (\q -> _Pos__compare_cont r p q)
      (\_ -> Datatypes.Gt)
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Datatypes.Lt)
      (\_ -> Datatypes.Lt)
      (\_ -> r)
      y)
    x

_Pos__compare :: Prelude.Integer -> Prelude.Integer ->
                 Datatypes.Coq_comparison
_Pos__compare =
  _Pos__compare_cont Datatypes.Eq

_Pos__min :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__min p p' =
  case _Pos__compare p p' of {
   Datatypes.Gt -> p';
   _ -> p}

_Pos__max :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__max p p' =
  case _Pos__compare p p' of {
   Datatypes.Gt -> p;
   _ -> p'}

_Pos__eqb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__eqb p q =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__eqb p0 q0)
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      q)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\q0 -> _Pos__eqb p0 q0)
      (\_ -> Prelude.False)
      q)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      q)
    p

_Pos__leb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__leb x y =
  case _Pos__compare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

_Pos__ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__ltb x y =
  case _Pos__compare x y of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

_Pos__sqrtrem_step :: (Prelude.Integer -> Prelude.Integer) ->
                      (Prelude.Integer -> Prelude.Integer) -> ((,)
                      Prelude.Integer Pos__Coq_mask) -> (,) Prelude.Integer
                      Pos__Coq_mask
_Pos__sqrtrem_step f g p =
  case p of {
   (,) s y ->
    case y of {
     Pos__IsPos r ->
      let {s' = (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) s)}
      in
      let {r' = g (f r)} in
      case _Pos__leb s' r' of {
       Prelude.True -> (,) ((\x -> 2 Prelude.* x Prelude.+ 1) s)
        (_Pos__sub_mask r' s');
       Prelude.False -> (,) ((\x -> 2 Prelude.* x) s) (Pos__IsPos r')};
     _ -> (,) ((\x -> 2 Prelude.* x) s)
      (_Pos__sub_mask (g (f 1)) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
        1)))}}

_Pos__sqrtrem :: Prelude.Integer -> (,) Prelude.Integer Pos__Coq_mask
_Pos__sqrtrem p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p1 ->
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x Prelude.+ 1) x) (\x ->
        (\x -> 2 Prelude.* x Prelude.+ 1) x) (_Pos__sqrtrem p1))
      (\p1 ->
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x) x) (\x ->
        (\x -> 2 Prelude.* x Prelude.+ 1) x) (_Pos__sqrtrem p1))
      (\_ -> (,) 1 (Pos__IsPos ((\x -> 2 Prelude.* x) 1)))
      p0)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p1 ->
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x Prelude.+ 1) x) (\x ->
        (\x -> 2 Prelude.* x) x) (_Pos__sqrtrem p1))
      (\p1 ->
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x) x) (\x ->
        (\x -> 2 Prelude.* x) x) (_Pos__sqrtrem p1))
      (\_ -> (,) 1 (Pos__IsPos 1))
      p0)
    (\_ -> (,) 1 Pos__IsNul)
    p

_Pos__sqrt :: Prelude.Integer -> Prelude.Integer
_Pos__sqrt p =
  Prelude.fst (_Pos__sqrtrem p)

_Pos__gcdn :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
              Prelude.Integer
_Pos__gcdn n a b =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 1)
    (\n0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\a' ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\b' ->
        case _Pos__compare a' b' of {
         Datatypes.Eq -> a;
         Datatypes.Lt -> _Pos__gcdn n0 (_Pos__sub b' a') a;
         Datatypes.Gt -> _Pos__gcdn n0 (_Pos__sub a' b') b})
        (\b0 -> _Pos__gcdn n0 a b0)
        (\_ -> 1)
        b)
      (\a0 ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\_ -> _Pos__gcdn n0 a0 b)
        (\b0 -> (\x -> 2 Prelude.* x) (_Pos__gcdn n0 a0 b0))
        (\_ -> 1)
        b)
      (\_ -> 1)
      a)
    n

_Pos__gcd :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__gcd a b =
  _Pos__gcdn ((Prelude.+) (_Pos__size_nat a) (_Pos__size_nat b)) a b

_Pos__ggcdn :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> (,)
               Prelude.Integer ((,) Prelude.Integer Prelude.Integer)
_Pos__ggcdn n a b =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> (,) 1 ((,) a b))
    (\n0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\a' ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\b' ->
        case _Pos__compare a' b' of {
         Datatypes.Eq -> (,) a ((,) 1 1);
         Datatypes.Lt ->
          case _Pos__ggcdn n0 (_Pos__sub b' a') a of {
           (,) g p ->
            case p of {
             (,) ba aa -> (,) g ((,) aa
              (_Pos__add aa ((\x -> 2 Prelude.* x) ba)))}};
         Datatypes.Gt ->
          case _Pos__ggcdn n0 (_Pos__sub a' b') b of {
           (,) g p ->
            case p of {
             (,) ab bb -> (,) g ((,)
              (_Pos__add bb ((\x -> 2 Prelude.* x) ab)) bb)}}})
        (\b0 ->
        case _Pos__ggcdn n0 a b0 of {
         (,) g p ->
          case p of {
           (,) aa bb -> (,) g ((,) aa ((\x -> 2 Prelude.* x) bb))}})
        (\_ -> (,) 1 ((,) a 1))
        b)
      (\a0 ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\_ ->
        case _Pos__ggcdn n0 a0 b of {
         (,) g p ->
          case p of {
           (,) aa bb -> (,) g ((,) ((\x -> 2 Prelude.* x) aa) bb)}})
        (\b0 ->
        case _Pos__ggcdn n0 a0 b0 of {
         (,) g p -> (,) ((\x -> 2 Prelude.* x) g) p})
        (\_ -> (,) 1 ((,) a 1))
        b)
      (\_ -> (,) 1 ((,) 1 b))
      a)
    n

_Pos__ggcd :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
              ((,) Prelude.Integer Prelude.Integer)
_Pos__ggcd a b =
  _Pos__ggcdn ((Prelude.+) (_Pos__size_nat a) (_Pos__size_nat b)) a b

_Pos__coq_Nsucc_double :: BinNums.N -> BinNums.N
_Pos__coq_Nsucc_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos 1;
   BinNums.Npos p -> BinNums.Npos ((\x -> 2 Prelude.* x Prelude.+ 1) p)}

_Pos__coq_Ndouble :: BinNums.N -> BinNums.N
_Pos__coq_Ndouble n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos ((\x -> 2 Prelude.* x) p)}

_Pos__lor :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__lor p q =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__lor p0 q0))
      (\q0 -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__lor p0 q0))
      (\_ -> p)
      q)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> (\x -> 2 Prelude.* x Prelude.+ 1) (_Pos__lor p0 q0))
      (\q0 -> (\x -> 2 Prelude.* x) (_Pos__lor p0 q0))
      (\_ -> (\x -> 2 Prelude.* x Prelude.+ 1) p0)
      q)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> q)
      (\q0 -> (\x -> 2 Prelude.* x Prelude.+ 1) q0)
      (\_ -> q)
      q)
    p

_Pos__land :: Prelude.Integer -> Prelude.Integer -> BinNums.N
_Pos__land p q =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__coq_Nsucc_double (_Pos__land p0 q0))
      (\q0 -> _Pos__coq_Ndouble (_Pos__land p0 q0))
      (\_ -> BinNums.Npos 1)
      q)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__coq_Ndouble (_Pos__land p0 q0))
      (\q0 -> _Pos__coq_Ndouble (_Pos__land p0 q0))
      (\_ -> BinNums.N0)
      q)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> BinNums.Npos 1)
      (\_ -> BinNums.N0)
      (\_ -> BinNums.Npos 1)
      q)
    p

_Pos__ldiff :: Prelude.Integer -> Prelude.Integer -> BinNums.N
_Pos__ldiff p q =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__coq_Ndouble (_Pos__ldiff p0 q0))
      (\q0 -> _Pos__coq_Nsucc_double (_Pos__ldiff p0 q0))
      (\_ -> BinNums.Npos ((\x -> 2 Prelude.* x) p0))
      q)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__coq_Ndouble (_Pos__ldiff p0 q0))
      (\q0 -> _Pos__coq_Ndouble (_Pos__ldiff p0 q0))
      (\_ -> BinNums.Npos p)
      q)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> BinNums.N0)
      (\_ -> BinNums.Npos 1)
      (\_ -> BinNums.N0)
      q)
    p

_Pos__lxor :: Prelude.Integer -> Prelude.Integer -> BinNums.N
_Pos__lxor p q =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__coq_Ndouble (_Pos__lxor p0 q0))
      (\q0 -> _Pos__coq_Nsucc_double (_Pos__lxor p0 q0))
      (\_ -> BinNums.Npos ((\x -> 2 Prelude.* x) p0))
      q)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> _Pos__coq_Nsucc_double (_Pos__lxor p0 q0))
      (\q0 -> _Pos__coq_Ndouble (_Pos__lxor p0 q0))
      (\_ -> BinNums.Npos ((\x -> 2 Prelude.* x Prelude.+ 1) p0))
      q)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q0 -> BinNums.Npos ((\x -> 2 Prelude.* x) q0))
      (\q0 -> BinNums.Npos ((\x -> 2 Prelude.* x Prelude.+ 1) q0))
      (\_ -> BinNums.N0)
      q)
    p

_Pos__shiftl_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__shiftl_nat p =
  Datatypes.nat_rect p (\_ x -> (\x -> 2 Prelude.* x) x)

_Pos__shiftr_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Pos__shiftr_nat p =
  Datatypes.nat_rect p (\_ -> _Pos__div2)

_Pos__shiftl :: Prelude.Integer -> BinNums.N -> Prelude.Integer
_Pos__shiftl p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Pos__iter (\x -> (\x -> 2 Prelude.* x) x) p n0}

_Pos__shiftr :: Prelude.Integer -> BinNums.N -> Prelude.Integer
_Pos__shiftr p n =
  case n of {
   BinNums.N0 -> p;
   BinNums.Npos n0 -> _Pos__iter _Pos__div2 p n0}

_Pos__testbit_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__testbit_nat p n =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.True)
      (\n' -> _Pos__testbit_nat p0 n')
      n)
    (\p0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\n' -> _Pos__testbit_nat p0 n')
      n)
    (\_ ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      n)
    p

_Pos__testbit :: Prelude.Integer -> BinNums.N -> Prelude.Bool
_Pos__testbit p n =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    case n of {
     BinNums.N0 -> Prelude.True;
     BinNums.Npos n0 -> _Pos__testbit p0 (_Pos__pred_N n0)})
    (\p0 ->
    case n of {
     BinNums.N0 -> Prelude.False;
     BinNums.Npos n0 -> _Pos__testbit p0 (_Pos__pred_N n0)})
    (\_ ->
    case n of {
     BinNums.N0 -> Prelude.True;
     BinNums.Npos _ -> Prelude.False})
    p

_Pos__iter_op :: (a1 -> a1 -> a1) -> Prelude.Integer -> a1 -> a1
_Pos__iter_op op p a =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> op a (_Pos__iter_op op p0 (op a a)))
    (\p0 -> _Pos__iter_op op p0 (op a a))
    (\_ -> a)
    p

_Pos__to_nat :: Prelude.Integer -> Prelude.Integer
_Pos__to_nat x =
  _Pos__iter_op (Prelude.+) x (Prelude.succ 0)

_Pos__of_nat :: Prelude.Integer -> Prelude.Integer
_Pos__of_nat n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 1)
    (\x ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> 1)
      (\_ -> _Pos__succ (_Pos__of_nat x))
      x)
    n

_Pos__of_succ_nat :: Prelude.Integer -> Prelude.Integer
_Pos__of_succ_nat n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 1)
    (\x -> _Pos__succ (_Pos__of_succ_nat x))
    n

_Pos__of_uint_acc :: Decimal.Coq_uint -> Prelude.Integer -> Prelude.Integer
_Pos__of_uint_acc d acc =
  case d of {
   Decimal.Nil -> acc;
   Decimal.D0 l ->
    _Pos__of_uint_acc l
      (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
        ((\x -> 2 Prelude.* x) 1))) acc);
   Decimal.D1 l ->
    _Pos__of_uint_acc l
      (_Pos__add 1
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D2 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) 1)
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D3 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) 1)
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D4 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D5 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
        1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D6 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
        1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D7 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1)
        ((\x -> 2 Prelude.* x Prelude.+ 1) 1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D8 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x) 1)))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc));
   Decimal.D9 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x) 1)))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
          ((\x -> 2 Prelude.* x) 1))) acc))}

_Pos__of_uint :: Decimal.Coq_uint -> BinNums.N
_Pos__of_uint d =
  case d of {
   Decimal.Nil -> BinNums.N0;
   Decimal.D0 l -> _Pos__of_uint l;
   Decimal.D1 l -> BinNums.Npos (_Pos__of_uint_acc l 1);
   Decimal.D2 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) 1));
   Decimal.D3 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1) 1));
   Decimal.D4 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)));
   Decimal.D5 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) 1)));
   Decimal.D6 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1)));
   Decimal.D7 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1)));
   Decimal.D8 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))));
   Decimal.D9 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))}

_Pos__of_int :: Decimal.Coq_int -> Prelude.Maybe Prelude.Integer
_Pos__of_int d =
  case d of {
   Decimal.Pos d0 ->
    case _Pos__of_uint d0 of {
     BinNums.N0 -> Prelude.Nothing;
     BinNums.Npos p -> Prelude.Just p};
   Decimal.Neg _ -> Prelude.Nothing}

_Pos__to_little_uint :: Prelude.Integer -> Decimal.Coq_uint
_Pos__to_little_uint p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> Decimal._Little__succ_double (_Pos__to_little_uint p0))
    (\p0 -> Decimal._Little__double (_Pos__to_little_uint p0))
    (\_ -> Decimal.D1 Decimal.Nil)
    p

_Pos__to_uint :: Prelude.Integer -> Decimal.Coq_uint
_Pos__to_uint p =
  Decimal.rev (_Pos__to_little_uint p)

_Pos__to_int :: Prelude.Integer -> Decimal.Coq_int
_Pos__to_int n =
  Decimal.Pos (_Pos__to_uint n)

_Pos__eq_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__eq_dec x y =
  BinNums.positive_rec (\_ h x0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (h p0))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      x0) (\_ h x0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\p0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (h p0))
      (\_ -> Prelude.False)
      x0) (\x0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      x0) x y

_Pos__peano_rect :: a1 -> (Prelude.Integer -> a1 -> a1) -> Prelude.Integer ->
                    a1
_Pos__peano_rect a f p =
  let {
   f2 = _Pos__peano_rect (f 1 a) (\p0 x ->
          f (_Pos__succ ((\x -> 2 Prelude.* x) p0))
            (f ((\x -> 2 Prelude.* x) p0) x))}
  in
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\q -> f ((\x -> 2 Prelude.* x) q) (f2 q))
    (\q -> f2 q)
    (\_ -> a)
    p

_Pos__peano_rec :: a1 -> (Prelude.Integer -> a1 -> a1) -> Prelude.Integer ->
                   a1
_Pos__peano_rec =
  _Pos__peano_rect

data Pos__PeanoView =
   Pos__PeanoOne
 | Pos__PeanoSucc Prelude.Integer Pos__PeanoView

_Pos__coq_PeanoView_rect :: a1 -> (Prelude.Integer -> Pos__PeanoView -> a1 ->
                            a1) -> Prelude.Integer -> Pos__PeanoView -> a1
_Pos__coq_PeanoView_rect f f0 _ p =
  case p of {
   Pos__PeanoOne -> f;
   Pos__PeanoSucc p0 p1 -> f0 p0 p1 (_Pos__coq_PeanoView_rect f f0 p0 p1)}

_Pos__coq_PeanoView_rec :: a1 -> (Prelude.Integer -> Pos__PeanoView -> a1 ->
                           a1) -> Prelude.Integer -> Pos__PeanoView -> a1
_Pos__coq_PeanoView_rec =
  _Pos__coq_PeanoView_rect

_Pos__peanoView_xO :: Prelude.Integer -> Pos__PeanoView -> Pos__PeanoView
_Pos__peanoView_xO _ q =
  case q of {
   Pos__PeanoOne -> Pos__PeanoSucc 1 Pos__PeanoOne;
   Pos__PeanoSucc p q0 -> Pos__PeanoSucc
    (_Pos__succ ((\x -> 2 Prelude.* x) p)) (Pos__PeanoSucc
    ((\x -> 2 Prelude.* x) p) (_Pos__peanoView_xO p q0))}

_Pos__peanoView_xI :: Prelude.Integer -> Pos__PeanoView -> Pos__PeanoView
_Pos__peanoView_xI _ q =
  case q of {
   Pos__PeanoOne -> Pos__PeanoSucc (_Pos__succ 1) (Pos__PeanoSucc 1
    Pos__PeanoOne);
   Pos__PeanoSucc p q0 -> Pos__PeanoSucc
    (_Pos__succ ((\x -> 2 Prelude.* x Prelude.+ 1) p)) (Pos__PeanoSucc
    ((\x -> 2 Prelude.* x Prelude.+ 1) p) (_Pos__peanoView_xI p q0))}

_Pos__peanoView :: Prelude.Integer -> Pos__PeanoView
_Pos__peanoView p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> _Pos__peanoView_xI p0 (_Pos__peanoView p0))
    (\p0 -> _Pos__peanoView_xO p0 (_Pos__peanoView p0))
    (\_ -> Pos__PeanoOne)
    p

_Pos__coq_PeanoView_iter :: a1 -> (Prelude.Integer -> a1 -> a1) ->
                            Prelude.Integer -> Pos__PeanoView -> a1
_Pos__coq_PeanoView_iter a f _ q =
  case q of {
   Pos__PeanoOne -> a;
   Pos__PeanoSucc p q0 -> f p (_Pos__coq_PeanoView_iter a f p q0)}

_Pos__eqb_spec :: Prelude.Integer -> Prelude.Integer -> Bool.Coq_reflect
_Pos__eqb_spec x y =
  Bool.iff_reflect (_Pos__eqb x y)

_Pos__switch_Eq :: Datatypes.Coq_comparison -> Datatypes.Coq_comparison ->
                   Datatypes.Coq_comparison
_Pos__switch_Eq c c' =
  case c' of {
   Datatypes.Eq -> c;
   x -> x}

_Pos__mask2cmp :: Pos__Coq_mask -> Datatypes.Coq_comparison
_Pos__mask2cmp p =
  case p of {
   Pos__IsNul -> Datatypes.Eq;
   Pos__IsPos _ -> Datatypes.Gt;
   Pos__IsNeg -> Datatypes.Lt}

_Pos__leb_spec0 :: Prelude.Integer -> Prelude.Integer -> Bool.Coq_reflect
_Pos__leb_spec0 x y =
  Bool.iff_reflect (_Pos__leb x y)

_Pos__ltb_spec0 :: Prelude.Integer -> Prelude.Integer -> Bool.Coq_reflect
_Pos__ltb_spec0 x y =
  Bool.iff_reflect (_Pos__ltb x y)

_Pos__Private_Dec__max_case_strong :: Prelude.Integer -> Prelude.Integer ->
                                      (Prelude.Integer -> Prelude.Integer ->
                                      () -> a1 -> a1) -> (() -> a1) -> (() ->
                                      a1) -> a1
_Pos__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (_Pos__max n m) __ (hl __);
   _ -> compat m (_Pos__max n m) __ (hr __)}

_Pos__Private_Dec__max_case :: Prelude.Integer -> Prelude.Integer ->
                               (Prelude.Integer -> Prelude.Integer -> () ->
                               a1 -> a1) -> a1 -> a1 -> a1
_Pos__Private_Dec__max_case n m x x0 x1 =
  _Pos__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Pos__Private_Dec__max_dec :: Prelude.Integer -> Prelude.Integer ->
                              Prelude.Bool
_Pos__Private_Dec__max_dec n m =
  _Pos__Private_Dec__max_case n m (\_ _ _ h0 -> h0) Prelude.True
    Prelude.False

_Pos__Private_Dec__min_case_strong :: Prelude.Integer -> Prelude.Integer ->
                                      (Prelude.Integer -> Prelude.Integer ->
                                      () -> a1 -> a1) -> (() -> a1) -> (() ->
                                      a1) -> a1
_Pos__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Pos__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (_Pos__min n m) __ (hr __);
   _ -> compat n (_Pos__min n m) __ (hl __)}

_Pos__Private_Dec__min_case :: Prelude.Integer -> Prelude.Integer ->
                               (Prelude.Integer -> Prelude.Integer -> () ->
                               a1 -> a1) -> a1 -> a1 -> a1
_Pos__Private_Dec__min_case n m x x0 x1 =
  _Pos__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Pos__Private_Dec__min_dec :: Prelude.Integer -> Prelude.Integer ->
                              Prelude.Bool
_Pos__Private_Dec__min_dec n m =
  _Pos__Private_Dec__min_case n m (\_ _ _ h0 -> h0) Prelude.True
    Prelude.False

_Pos__max_case_strong :: Prelude.Integer -> Prelude.Integer -> (() -> a1) ->
                         (() -> a1) -> a1
_Pos__max_case_strong n m x x0 =
  _Pos__Private_Dec__max_case_strong n m (\_ _ _ x1 ->
    Logic.eq_rect __ x1 __) x x0

_Pos__max_case :: Prelude.Integer -> Prelude.Integer -> a1 -> a1 -> a1
_Pos__max_case n m x x0 =
  _Pos__max_case_strong n m (\_ -> x) (\_ -> x0)

_Pos__max_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__max_dec =
  _Pos__Private_Dec__max_dec

_Pos__min_case_strong :: Prelude.Integer -> Prelude.Integer -> (() -> a1) ->
                         (() -> a1) -> a1
_Pos__min_case_strong n m x x0 =
  _Pos__Private_Dec__min_case_strong n m (\_ _ _ x1 ->
    Logic.eq_rect __ x1 __) x x0

_Pos__min_case :: Prelude.Integer -> Prelude.Integer -> a1 -> a1 -> a1
_Pos__min_case n m x x0 =
  _Pos__min_case_strong n m (\_ -> x) (\_ -> x0)

_Pos__min_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__min_dec =
  _Pos__Private_Dec__min_dec

