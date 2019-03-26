module BinPos where

import qualified Prelude
import qualified BinNums
import qualified Datatypes
import qualified Decimal
import qualified Specif

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

data Pos__Coq_mask =
   Pos__IsNul
 | Pos__IsPos Prelude.Integer
 | Pos__IsNeg

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
    (\p -> Pos__IsPos ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) p)))
    (\p -> Pos__IsPos ((\x -> 2 Prelude.* x) (_Pos__pred_double p)))
    (\_ -> Pos__IsNul)
    x

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

_Pos__square :: Prelude.Integer -> Prelude.Integer
_Pos__square p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    (_Pos__add (_Pos__square p0) p0)))
    (\p0 -> (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) (_Pos__square p0)))
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

_Pos__compare_cont :: Datatypes.Coq_comparison -> Prelude.Integer -> Prelude.Integer ->
                      Datatypes.Coq_comparison
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

_Pos__compare :: Prelude.Integer -> Prelude.Integer -> Datatypes.Coq_comparison
_Pos__compare =
  _Pos__compare_cont Datatypes.Eq

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

_Pos__sqrtrem_step :: (Prelude.Integer -> Prelude.Integer) -> (Prelude.Integer -> Prelude.Integer) ->
                      ((,) Prelude.Integer Pos__Coq_mask) -> (,) Prelude.Integer Pos__Coq_mask
_Pos__sqrtrem_step f g p =
  case p of {
   (,) s y ->
    case y of {
     Pos__IsPos r ->
      let {s' = (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) s)} in
      let {r' = g (f r)} in
      case _Pos__leb s' r' of {
       Prelude.True -> (,) ((\x -> 2 Prelude.* x Prelude.+ 1) s) (_Pos__sub_mask r' s');
       Prelude.False -> (,) ((\x -> 2 Prelude.* x) s) (Pos__IsPos r')};
     _ -> (,) ((\x -> 2 Prelude.* x) s)
      (_Pos__sub_mask (g (f 1)) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))}}

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
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x) x) (\x -> (\x -> 2 Prelude.* x Prelude.+ 1) x)
        (_Pos__sqrtrem p1))
      (\_ -> (,) 1 (Pos__IsPos ((\x -> 2 Prelude.* x) 1)))
      p0)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p1 ->
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x Prelude.+ 1) x) (\x -> (\x -> 2 Prelude.* x) x)
        (_Pos__sqrtrem p1))
      (\p1 ->
      _Pos__sqrtrem_step (\x -> (\x -> 2 Prelude.* x) x) (\x -> (\x -> 2 Prelude.* x) x)
        (_Pos__sqrtrem p1))
      (\_ -> (,) 1 (Pos__IsPos 1))
      p0)
    (\_ -> (,) 1 Pos__IsNul)
    p

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
      (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
        1))) acc);
   Decimal.D1 l ->
    _Pos__of_uint_acc l
      (_Pos__add 1
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D2 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) 1)
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D3 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) 1)
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D4 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D5 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D6 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D7 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D8 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc));
   Decimal.D9 l ->
    _Pos__of_uint_acc l
      (_Pos__add ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
        1)))
        (_Pos__mul ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
          1))) acc))}

_Pos__of_uint :: Decimal.Coq_uint -> BinNums.N
_Pos__of_uint d =
  case d of {
   Decimal.Nil -> BinNums.N0;
   Decimal.D0 l -> _Pos__of_uint l;
   Decimal.D1 l -> BinNums.Npos (_Pos__of_uint_acc l 1);
   Decimal.D2 l -> BinNums.Npos (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) 1));
   Decimal.D3 l -> BinNums.Npos (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1) 1));
   Decimal.D4 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)));
   Decimal.D5 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1)));
   Decimal.D6 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)));
   Decimal.D7 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)));
   Decimal.D8 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))));
   Decimal.D9 l -> BinNums.Npos
    (_Pos__of_uint_acc l ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))))}

_Pos__eq_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Pos__eq_dec x y =
  BinNums.positive_rec (\_ h x0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p0 -> Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (h p0))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      x0) (\_ h x0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\p0 -> Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (h p0))
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

