module BinInt where

import qualified Prelude
import qualified BinNums
import qualified BinPos
import qualified Datatypes
import qualified Decimal
import qualified Specif

_Z__double :: Prelude.Integer -> Prelude.Integer
_Z__double x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) ((\x -> 2 Prelude.* x) p))
    (\p -> Prelude.negate ((\x -> 2 Prelude.* x) p))
    x

_Z__succ_double :: Prelude.Integer -> Prelude.Integer
_Z__succ_double x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (\x -> x) 1)
    (\p -> (\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) p))
    (\p -> Prelude.negate (BinPos._Pos__pred_double p))
    x

_Z__pred_double :: Prelude.Integer -> Prelude.Integer
_Z__pred_double x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Prelude.negate 1)
    (\p -> (\x -> x) (BinPos._Pos__pred_double p))
    (\p -> Prelude.negate ((\x -> 2 Prelude.* x Prelude.+ 1) p))
    x

_Z__pos_sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__pos_sub x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Z__double (_Z__pos_sub p q))
      (\q -> _Z__succ_double (_Z__pos_sub p q))
      (\_ -> (\x -> x) ((\x -> 2 Prelude.* x) p))
      y)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> _Z__pred_double (_Z__pos_sub p q))
      (\q -> _Z__double (_Z__pos_sub p q))
      (\_ -> (\x -> x) (BinPos._Pos__pred_double p))
      y)
    (\_ ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> Prelude.negate ((\x -> 2 Prelude.* x) q))
      (\q -> Prelude.negate (BinPos._Pos__pred_double q))
      (\_ -> 0)
      y)
    x

_Z__opp :: Prelude.Integer -> Prelude.Integer
_Z__opp x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\x0 -> Prelude.negate x0)
    (\x0 -> (\x -> x) x0)
    x

_Z__succ :: Prelude.Integer -> Prelude.Integer
_Z__succ x =
  (Prelude.+) x ((\x -> x) 1)

_Z__pred :: Prelude.Integer -> Prelude.Integer
_Z__pred x =
  (Prelude.+) x (Prelude.negate 1)

_Z__pow_pos :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__pow_pos z =
  BinPos._Pos__iter ((Prelude.*) z) ((\x -> x) 1)

_Z__pow :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__pow x y =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (\x -> x) 1)
    (\p -> _Z__pow_pos x p)
    (\_ -> 0)
    y

_Z__compare :: Prelude.Integer -> Prelude.Integer -> Datatypes.Coq_comparison
_Z__compare x y =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Datatypes.Eq)
      (\_ -> Datatypes.Lt)
      (\_ -> Datatypes.Gt)
      y)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Datatypes.Gt)
      (\y' -> BinPos._Pos__compare x' y')
      (\_ -> Datatypes.Gt)
      y)
    (\x' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Datatypes.Lt)
      (\_ -> Datatypes.Lt)
      (\y' -> Datatypes.coq_CompOpp (BinPos._Pos__compare x' y'))
      y)
    x

_Z__leb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__leb x y =
  case _Z__compare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

_Z__ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__ltb x y =
  case _Z__compare x y of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

_Z__eqb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__eqb x y =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      y)
    (\p ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\q -> BinPos._Pos__eqb p q)
      (\_ -> Prelude.False)
      y)
    (\p ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\q -> BinPos._Pos__eqb p q)
      y)
    x

_Z__of_nat :: Prelude.Integer -> Prelude.Integer
_Z__of_nat n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\n0 -> (\x -> x) (BinPos._Pos__of_succ_nat n0))
    n

_Z__of_N :: BinNums.N -> Prelude.Integer
_Z__of_N n =
  case n of {
   BinNums.N0 -> 0;
   BinNums.Npos p -> (\x -> x) p}

_Z__of_uint :: Decimal.Coq_uint -> Prelude.Integer
_Z__of_uint d =
  _Z__of_N (BinPos._Pos__of_uint d)

_Z__of_int :: Decimal.Coq_int -> Prelude.Integer
_Z__of_int d =
  case d of {
   Decimal.Pos d0 -> _Z__of_uint d0;
   Decimal.Neg d0 -> _Z__opp (_Z__of_uint d0)}

_Z__pos_div_eucl :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer Prelude.Integer
_Z__pos_div_eucl a b =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\a' ->
    case _Z__pos_div_eucl a' b of {
     (,) q r ->
      let {r' = (Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) r) ((\x -> x) 1)} in
      case _Z__ltb r' b of {
       Prelude.True -> (,) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q) r';
       Prelude.False -> (,)
        ((Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q) ((\x -> x) 1))
        ((Prelude.-) r' b)}})
    (\a' ->
    case _Z__pos_div_eucl a' b of {
     (,) q r ->
      let {r' = (Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) r} in
      case _Z__ltb r' b of {
       Prelude.True -> (,) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q) r';
       Prelude.False -> (,)
        ((Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q) ((\x -> x) 1))
        ((Prelude.-) r' b)}})
    (\_ ->
    case _Z__leb ((\x -> x) ((\x -> 2 Prelude.* x) 1)) b of {
     Prelude.True -> (,) 0 ((\x -> x) 1);
     Prelude.False -> (,) ((\x -> x) 1) 0})
    a

_Z__div_eucl :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer Prelude.Integer
_Z__div_eucl a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) 0 0)
    (\a' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) 0 0)
      (\_ -> _Z__pos_div_eucl a' b)
      (\b' ->
      case _Z__pos_div_eucl a' ((\x -> x) b') of {
       (,) q r ->
        (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
          (\_ -> (,) (_Z__opp q) 0)
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1))) ((Prelude.+) b r))
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1))) ((Prelude.+) b r))
          r})
      b)
    (\a' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) 0 0)
      (\_ ->
      case _Z__pos_div_eucl a' b of {
       (,) q r ->
        (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
          (\_ -> (,) (_Z__opp q) 0)
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1))) ((Prelude.-) b r))
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1))) ((Prelude.-) b r))
          r})
      (\b' -> case _Z__pos_div_eucl a' ((\x -> x) b') of {
               (,) q r -> (,) q (_Z__opp r)})
      b)
    a

_Z__div :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__div = (\n m -> if m Prelude.== 0 then 0 else Prelude.div n m)

_Z__modulo :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__modulo = (\n m -> if m Prelude.== 0 then 0 else Prelude.mod n m)

_Z__div2 :: Prelude.Integer -> Prelude.Integer
_Z__div2 z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> (\x -> x) (BinPos._Pos__div2 p))
      (\_ -> (\x -> x) (BinPos._Pos__div2 p))
      (\_ -> 0)
      p)
    (\p -> Prelude.negate (BinPos._Pos__div2_up p))
    z

_Z__log2 :: Prelude.Integer -> Prelude.Integer
_Z__log2 z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p -> (\x -> x) (BinPos._Pos__size p))
      (\p -> (\x -> x) (BinPos._Pos__size p))
      (\_ -> 0)
      p0)
    (\_ -> 0)
    z

_Z__sqrtrem :: Prelude.Integer -> (,) Prelude.Integer Prelude.Integer
_Z__sqrtrem n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) 0 0)
    (\p ->
    case BinPos._Pos__sqrtrem p of {
     (,) s m ->
      case m of {
       BinPos.Pos__IsPos r -> (,) ((\x -> x) s) ((\x -> x) r);
       _ -> (,) ((\x -> x) s) 0}})
    (\_ -> (,) 0 0)
    n

_Z__shiftl :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__shiftl a n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> a)
    (\p -> BinPos._Pos__iter ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1))) a p)
    (\p -> BinPos._Pos__iter _Z__div2 a p)
    n

_Z__eq_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__eq_dec x y =
  BinNums.coq_Z_rec (\x0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      x0) (\p x0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\p0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (BinPos._Pos__eq_dec p p0))
      (\_ -> Prelude.False)
      x0) (\p x0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\p0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (BinPos._Pos__eq_dec p p0))
      x0) x y

_Z__log2_up :: Prelude.Integer -> Prelude.Integer
_Z__log2_up a =
  case _Z__compare ((\x -> x) 1) a of {
   Datatypes.Lt -> _Z__succ (_Z__log2 (_Z__pred a));
   _ -> 0}

