module BinInt where

import qualified Prelude
import qualified BinNat
import qualified BinNums
import qualified BinPos
import qualified Bool
import qualified Datatypes
import qualified Decimal
import qualified Logic
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

type Z__Coq_t = Prelude.Integer

_Z__zero :: Prelude.Integer
_Z__zero =
  0

_Z__one :: Prelude.Integer
_Z__one =
  (\x -> x) 1

_Z__two :: Prelude.Integer
_Z__two =
  (\x -> x) ((\x -> 2 Prelude.* x) 1)

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

_Z__square :: Prelude.Integer -> Prelude.Integer
_Z__square x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) (BinPos._Pos__square p))
    (\p -> (\x -> x) (BinPos._Pos__square p))
    x

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

_Z__sgn :: Prelude.Integer -> Prelude.Integer
_Z__sgn z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\_ -> (\x -> x) 1)
    (\_ -> Prelude.negate 1)
    z

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

_Z__geb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__geb x y =
  case _Z__compare x y of {
   Datatypes.Lt -> Prelude.False;
   _ -> Prelude.True}

_Z__gtb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__gtb x y =
  case _Z__compare x y of {
   Datatypes.Gt -> Prelude.True;
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

_Z__abs :: Prelude.Integer -> Prelude.Integer
_Z__abs z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) p)
    (\p -> (\x -> x) p)
    z

_Z__abs_nat :: Prelude.Integer -> Prelude.Integer
_Z__abs_nat z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> BinPos._Pos__to_nat p)
    (\p -> BinPos._Pos__to_nat p)
    z

_Z__abs_N :: Prelude.Integer -> BinNums.N
_Z__abs_N z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> BinNums.N0)
    (\p -> BinNums.Npos p)
    (\p -> BinNums.Npos p)
    z

_Z__to_nat :: Prelude.Integer -> Prelude.Integer
_Z__to_nat z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> BinPos._Pos__to_nat p)
    (\_ -> 0)
    z

_Z__to_N :: Prelude.Integer -> BinNums.N
_Z__to_N z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> BinNums.N0)
    (\p -> BinNums.Npos p)
    (\_ -> BinNums.N0)
    z

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

_Z__to_pos :: Prelude.Integer -> Prelude.Integer
_Z__to_pos z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 1)
    (\p -> p)
    (\_ -> 1)
    z

_Z__of_uint :: Decimal.Coq_uint -> Prelude.Integer
_Z__of_uint d =
  _Z__of_N (BinPos._Pos__of_uint d)

_Z__of_int :: Decimal.Coq_int -> Prelude.Integer
_Z__of_int d =
  case d of {
   Decimal.Pos d0 -> _Z__of_uint d0;
   Decimal.Neg d0 -> _Z__opp (_Z__of_uint d0)}

_Z__to_int :: Prelude.Integer -> Decimal.Coq_int
_Z__to_int n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Decimal.Pos (Decimal.D0 Decimal.Nil))
    (\p -> Decimal.Pos (BinPos._Pos__to_uint p))
    (\p -> Decimal.Neg (BinPos._Pos__to_uint p))
    n

_Z__iter :: Prelude.Integer -> (a1 -> a1) -> a1 -> a1
_Z__iter n f x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> x)
    (\p -> BinPos._Pos__iter f x p)
    (\_ -> x)
    n

_Z__pos_div_eucl :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
                    Prelude.Integer
_Z__pos_div_eucl a b =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\a' ->
    case _Z__pos_div_eucl a' b of {
     (,) q r ->
      let {
       r' = (Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) r)
              ((\x -> x) 1)}
      in
      case _Z__ltb r' b of {
       Prelude.True -> (,) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q) r';
       Prelude.False -> (,)
        ((Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q)
          ((\x -> x) 1)) ((Prelude.-) r' b)}})
    (\a' ->
    case _Z__pos_div_eucl a' b of {
     (,) q r ->
      let {r' = (Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) r} in
      case _Z__ltb r' b of {
       Prelude.True -> (,) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q) r';
       Prelude.False -> (,)
        ((Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) q)
          ((\x -> x) 1)) ((Prelude.-) r' b)}})
    (\_ ->
    case _Z__leb ((\x -> x) ((\x -> 2 Prelude.* x) 1)) b of {
     Prelude.True -> (,) 0 ((\x -> x) 1);
     Prelude.False -> (,) ((\x -> x) 1) 0})
    a

_Z__div_eucl :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
                Prelude.Integer
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
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1)))
          ((Prelude.+) b r))
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
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1)))
          ((Prelude.-) b r))
          (\_ -> (,) (_Z__opp ((Prelude.+) q ((\x -> x) 1))) ((Prelude.-) b r))
          r})
      (\b' ->
      case _Z__pos_div_eucl a' ((\x -> x) b') of {
       (,) q r -> (,) q (_Z__opp r)})
      b)
    a

_Z__div :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__div = (\n m -> if m Prelude.== 0 then 0 else Prelude.div n m)

_Z__modulo :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__modulo = (\n m -> if m Prelude.== 0 then 0 else Prelude.mod n m)

_Z__quotrem :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
               Prelude.Integer
_Z__quotrem a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) 0 0)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) 0 a)
      (\b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       (,) q r -> (,) (_Z__of_N q) (_Z__of_N r)})
      (\b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       (,) q r -> (,) (_Z__opp (_Z__of_N q)) (_Z__of_N r)})
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) 0 a)
      (\b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       (,) q r -> (,) (_Z__opp (_Z__of_N q)) (_Z__opp (_Z__of_N r))})
      (\b0 ->
      case BinNat._N__pos_div_eucl a0 (BinNums.Npos b0) of {
       (,) q r -> (,) (_Z__of_N q) (_Z__opp (_Z__of_N r))})
      b)
    a

_Z__quot :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__quot a b =
  Prelude.fst (_Z__quotrem a b)

_Z__rem :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__rem a b =
  Prelude.snd (_Z__quotrem a b)

_Z__even :: Prelude.Integer -> Prelude.Bool
_Z__even z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Prelude.True)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      p)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      p)
    z

_Z__odd :: Prelude.Integer -> Prelude.Bool
_Z__odd z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Prelude.False)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      p)
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      p)
    z

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

_Z__quot2 :: Prelude.Integer -> Prelude.Integer
_Z__quot2 z =
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
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.negate (BinPos._Pos__div2 p))
      (\_ -> Prelude.negate (BinPos._Pos__div2 p))
      (\_ -> 0)
      p)
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

_Z__sqrt :: Prelude.Integer -> Prelude.Integer
_Z__sqrt n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) (BinPos._Pos__sqrt p))
    (\_ -> 0)
    n

_Z__gcd :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__gcd a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> _Z__abs b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> _Z__abs a)
      (\b0 -> (\x -> x) (BinPos._Pos__gcd a0 b0))
      (\b0 -> (\x -> x) (BinPos._Pos__gcd a0 b0))
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> _Z__abs a)
      (\b0 -> (\x -> x) (BinPos._Pos__gcd a0 b0))
      (\b0 -> (\x -> x) (BinPos._Pos__gcd a0 b0))
      b)
    a

_Z__ggcd :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
            ((,) Prelude.Integer Prelude.Integer)
_Z__ggcd a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) (_Z__abs b) ((,) 0 (_Z__sgn b)))
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) (_Z__abs a) ((,) (_Z__sgn a) 0))
      (\b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) ((\x -> x) g) ((,) ((\x -> x) aa) ((\x -> x) bb))}})
      (\b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) ((\x -> x) g) ((,) ((\x -> x) aa) (Prelude.negate bb))}})
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) (_Z__abs a) ((,) (_Z__sgn a) 0))
      (\b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) ((\x -> x) g) ((,) (Prelude.negate aa) ((\x -> x) bb))}})
      (\b0 ->
      case BinPos._Pos__ggcd a0 b0 of {
       (,) g p ->
        case p of {
         (,) aa bb -> (,) ((\x -> x) g) ((,) (Prelude.negate aa) (Prelude.negate
          bb))}})
      b)
    a

_Z__testbit :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__testbit a n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> _Z__odd a)
    (\p ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\a0 -> BinPos._Pos__testbit a0 (BinNums.Npos p))
      (\a0 ->
      Prelude.not (BinNat._N__testbit (BinPos._Pos__pred_N a0) (BinNums.Npos p)))
      a)
    (\_ -> Prelude.False)
    n

_Z__shiftl :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__shiftl a n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> a)
    (\p ->
    BinPos._Pos__iter ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1))) a p)
    (\p -> BinPos._Pos__iter _Z__div2 a p)
    n

_Z__shiftr :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__shiftr a n =
  _Z__shiftl a (_Z__opp n)

_Z__lor :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__lor a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> a)
      (\b0 -> (\x -> x) (BinPos._Pos__lor a0 b0))
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinNums.Npos a0))))
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> a)
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__ldiff (BinPos._Pos__pred_N a0) (BinNums.Npos b0))))
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__land (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0))))
      b)
    a

_Z__land :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__land a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> 0)
      (\b0 -> _Z__of_N (BinPos._Pos__land a0 b0))
      (\b0 ->
      _Z__of_N (BinNat._N__ldiff (BinNums.Npos a0) (BinPos._Pos__pred_N b0)))
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> 0)
      (\b0 ->
      _Z__of_N (BinNat._N__ldiff (BinNums.Npos b0) (BinPos._Pos__pred_N a0)))
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0))))
      b)
    a

_Z__ldiff :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__ldiff a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> a)
      (\b0 -> _Z__of_N (BinPos._Pos__ldiff a0 b0))
      (\b0 ->
      _Z__of_N (BinNat._N__land (BinNums.Npos a0) (BinPos._Pos__pred_N b0)))
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> a)
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__lor (BinPos._Pos__pred_N a0) (BinNums.Npos b0))))
      (\b0 ->
      _Z__of_N (BinNat._N__ldiff (BinPos._Pos__pred_N b0) (BinPos._Pos__pred_N a0)))
      b)
    a

_Z__lxor :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__lxor a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> a)
      (\b0 -> _Z__of_N (BinPos._Pos__lxor a0 b0))
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__lxor (BinNums.Npos a0) (BinPos._Pos__pred_N b0))))
      b)
    (\a0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> a)
      (\b0 -> Prelude.negate
      (BinNat._N__succ_pos
        (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinNums.Npos b0))))
      (\b0 ->
      _Z__of_N (BinNat._N__lxor (BinPos._Pos__pred_N a0) (BinPos._Pos__pred_N b0)))
      b)
    a

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
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False)
        (BinPos._Pos__eq_dec p p0))
      (\_ -> Prelude.False)
      x0) (\p x0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.False)
      (\_ -> Prelude.False)
      (\p0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False)
        (BinPos._Pos__eq_dec p p0))
      x0) x y

_Z__leb_spec0 :: Prelude.Integer -> Prelude.Integer -> Bool.Coq_reflect
_Z__leb_spec0 x y =
  Bool.iff_reflect (_Z__leb x y)

_Z__ltb_spec0 :: Prelude.Integer -> Prelude.Integer -> Bool.Coq_reflect
_Z__ltb_spec0 x y =
  Bool.iff_reflect (_Z__ltb x y)

_Z__Private_Dec__max_case_strong :: Prelude.Integer -> Prelude.Integer ->
                                    (Prelude.Integer -> Prelude.Integer -> () -> a1
                                    -> a1) -> (() -> a1) -> (() -> a1) -> a1
_Z__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (Prelude.max n m) __ (hl __);
   _ -> compat m (Prelude.max n m) __ (hr __)}

_Z__Private_Dec__max_case :: Prelude.Integer -> Prelude.Integer -> (Prelude.Integer
                             -> Prelude.Integer -> () -> a1 -> a1) -> a1 -> a1 -> a1
_Z__Private_Dec__max_case n m x x0 x1 =
  _Z__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z__Private_Dec__max_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__Private_Dec__max_dec n m =
  _Z__Private_Dec__max_case n m (\_ _ _ h0 -> h0) Prelude.True Prelude.False

_Z__Private_Dec__min_case_strong :: Prelude.Integer -> Prelude.Integer ->
                                    (Prelude.Integer -> Prelude.Integer -> () -> a1
                                    -> a1) -> (() -> a1) -> (() -> a1) -> a1
_Z__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_Z__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (Prelude.min n m) __ (hr __);
   _ -> compat n (Prelude.min n m) __ (hl __)}

_Z__Private_Dec__min_case :: Prelude.Integer -> Prelude.Integer -> (Prelude.Integer
                             -> Prelude.Integer -> () -> a1 -> a1) -> a1 -> a1 -> a1
_Z__Private_Dec__min_case n m x x0 x1 =
  _Z__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_Z__Private_Dec__min_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__Private_Dec__min_dec n m =
  _Z__Private_Dec__min_case n m (\_ _ _ h0 -> h0) Prelude.True Prelude.False

_Z__max_case_strong :: Prelude.Integer -> Prelude.Integer -> (() -> a1) -> (() ->
                       a1) -> a1
_Z__max_case_strong n m x x0 =
  _Z__Private_Dec__max_case_strong n m (\_ _ _ x1 -> Logic.eq_rect __ x1 __) x x0

_Z__max_case :: Prelude.Integer -> Prelude.Integer -> a1 -> a1 -> a1
_Z__max_case n m x x0 =
  _Z__max_case_strong n m (\_ -> x) (\_ -> x0)

_Z__max_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__max_dec =
  _Z__Private_Dec__max_dec

_Z__min_case_strong :: Prelude.Integer -> Prelude.Integer -> (() -> a1) -> (() ->
                       a1) -> a1
_Z__min_case_strong n m x x0 =
  _Z__Private_Dec__min_case_strong n m (\_ _ _ x1 -> Logic.eq_rect __ x1 __) x x0

_Z__min_case :: Prelude.Integer -> Prelude.Integer -> a1 -> a1 -> a1
_Z__min_case n m x x0 =
  _Z__min_case_strong n m (\_ -> x) (\_ -> x0)

_Z__min_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Z__min_dec =
  _Z__Private_Dec__min_dec

_Z__sqrt_up :: Prelude.Integer -> Prelude.Integer
_Z__sqrt_up a =
  case _Z__compare 0 a of {
   Datatypes.Lt -> _Z__succ (_Z__sqrt (_Z__pred a));
   _ -> 0}

_Z__log2_up :: Prelude.Integer -> Prelude.Integer
_Z__log2_up a =
  case _Z__compare ((\x -> x) 1) a of {
   Datatypes.Lt -> _Z__succ (_Z__log2 (_Z__pred a));
   _ -> 0}

_Z__Private_Div__Quot2Div__div :: Prelude.Integer -> Prelude.Integer ->
                                  Prelude.Integer
_Z__Private_Div__Quot2Div__div =
  _Z__quot

_Z__Private_Div__Quot2Div__modulo :: Prelude.Integer -> Prelude.Integer ->
                                     Prelude.Integer
_Z__Private_Div__Quot2Div__modulo =
  _Z__rem

_Z__lcm :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__lcm a b =
  _Z__abs ((Prelude.*) a (_Z__div b (_Z__gcd a b)))

_Z__eqb_spec :: Prelude.Integer -> Prelude.Integer -> Bool.Coq_reflect
_Z__eqb_spec x y =
  Bool.iff_reflect (_Z__eqb x y)

_Z__b2z :: Prelude.Bool -> Prelude.Integer
_Z__b2z b =
  case b of {
   Prelude.True -> (\x -> x) 1;
   Prelude.False -> 0}

_Z__setbit :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__setbit a n =
  _Z__lor a (_Z__shiftl ((\x -> x) 1) n)

_Z__clearbit :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Z__clearbit a n =
  _Z__ldiff a (_Z__shiftl ((\x -> x) 1) n)

_Z__lnot :: Prelude.Integer -> Prelude.Integer
_Z__lnot a =
  _Z__pred (_Z__opp a)

_Z__ones :: Prelude.Integer -> Prelude.Integer
_Z__ones n =
  _Z__pred (_Z__shiftl ((\x -> x) 1) n)

