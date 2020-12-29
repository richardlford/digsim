module Fcore_Zaux where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified Datatypes

eq_dep_elim :: a1 -> a2 -> a1 -> a2
eq_dep_elim _ f _ =
  f

coq_Zeven :: Prelude.Integer -> Prelude.Bool
coq_Zeven n =
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
    n

type Coq_radix =
  Prelude.Integer
  -- singleton inductive, whose constructor was Build_radix
  
radix_val :: Coq_radix -> Prelude.Integer
radix_val r =
  r

radix2 :: Coq_radix
radix2 =
  (\x -> x) ((\x -> 2 Prelude.* x) 1)

cond_Zopp :: Prelude.Bool -> Prelude.Integer -> Prelude.Integer
cond_Zopp b m =
  case b of {
   Prelude.True -> BinInt._Z__opp m;
   Prelude.False -> m}

coq_Zfast_pow_pos :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
coq_Zfast_pow_pos v e =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\e' ->
    (Prelude.*) v (BinInt._Z__square (coq_Zfast_pow_pos v e')))
    (\e' -> BinInt._Z__square (coq_Zfast_pow_pos v e'))
    (\_ -> v)
    e

coq_Zpos_div_eucl_aux1 :: Prelude.Integer -> Prelude.Integer -> (,)
                          Prelude.Integer Prelude.Integer
coq_Zpos_div_eucl_aux1 a b =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\_ -> BinInt._Z__pos_div_eucl a ((\x -> x) b))
    (\b' ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\a' ->
      case coq_Zpos_div_eucl_aux1 a' b' of {
       (,) q r -> (,) q
        ((Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) r)
          ((\x -> x) 1))})
      (\a' ->
      case coq_Zpos_div_eucl_aux1 a' b' of {
       (,) q r -> (,) q ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) r)})
      (\_ -> (,) 0 ((\x -> x) a))
      a)
    (\_ -> (,) ((\x -> x) a) 0)
    b

coq_Zpos_div_eucl_aux :: Prelude.Integer -> Prelude.Integer -> (,)
                         Prelude.Integer Prelude.Integer
coq_Zpos_div_eucl_aux a b =
  case BinPos._Pos__compare a b of {
   Datatypes.Eq -> (,) ((\x -> x) 1) 0;
   Datatypes.Lt -> (,) 0 ((\x -> x) a);
   Datatypes.Gt -> coq_Zpos_div_eucl_aux1 a b}

coq_Zfast_div_eucl :: Prelude.Integer -> Prelude.Integer -> (,)
                      Prelude.Integer Prelude.Integer
coq_Zfast_div_eucl a b =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) 0 0)
    (\a' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) 0 0)
      (\b' -> coq_Zpos_div_eucl_aux a' b')
      (\b' ->
      case coq_Zpos_div_eucl_aux a' b' of {
       (,) q r ->
        (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
          (\_ -> (,) (BinInt._Z__opp q) 0)
          (\_ -> (,) (BinInt._Z__opp ((Prelude.+) q ((\x -> x) 1)))
          ((Prelude.+) b r))
          (\_ -> (,) (BinInt._Z__opp ((Prelude.+) q ((\x -> x) 1)))
          ((Prelude.+) b r))
          r})
      b)
    (\a' ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> (,) 0 0)
      (\b' ->
      case coq_Zpos_div_eucl_aux a' b' of {
       (,) q r ->
        (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
          (\_ -> (,) (BinInt._Z__opp q) 0)
          (\_ -> (,) (BinInt._Z__opp ((Prelude.+) q ((\x -> x) 1)))
          ((Prelude.-) b r))
          (\_ -> (,) (BinInt._Z__opp ((Prelude.+) q ((\x -> x) 1)))
          ((Prelude.-) b r))
          r})
      (\b' ->
      case coq_Zpos_div_eucl_aux a' b' of {
       (,) q r -> (,) q (BinInt._Z__opp r)})
      b)
    a

iter_nat :: (a1 -> a1) -> Prelude.Integer -> a1 -> a1
iter_nat f n x =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> x)
    (\n' -> iter_nat f n' (f x))
    n

iter_pos :: (a1 -> a1) -> Prelude.Integer -> a1 -> a1
iter_pos f n x =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\n' -> iter_pos f n' (iter_pos f n' (f x)))
    (\n' -> iter_pos f n' (iter_pos f n' x))
    (\_ -> f x)
    n

