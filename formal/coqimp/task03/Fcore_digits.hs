module Fcore_digits where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified Fcore_Zaux

digits2_Pnat :: Prelude.Integer -> Prelude.Integer
digits2_Pnat n =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> Prelude.succ (digits2_Pnat p))
    (\p -> Prelude.succ (digits2_Pnat p))
    (\_ -> 0)
    n

coq_Zdigit :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> Prelude.Integer ->
              Prelude.Integer
coq_Zdigit beta n k =
  BinInt._Z__rem (BinInt._Z__quot n (BinInt._Z__pow (Fcore_Zaux.radix_val beta) k))
    (Fcore_Zaux.radix_val beta)

coq_Zsum_digit :: Fcore_Zaux.Coq_radix -> (Prelude.Integer -> Prelude.Integer) ->
                  Prelude.Integer -> Prelude.Integer
coq_Zsum_digit beta f k =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\k0 ->
    (Prelude.+) (coq_Zsum_digit beta f k0)
      ((Prelude.*) (f (BinInt._Z__of_nat k0))
        (BinInt._Z__pow (Fcore_Zaux.radix_val beta) (BinInt._Z__of_nat k0))))
    k

coq_Zscale :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> Prelude.Integer ->
              Prelude.Integer
coq_Zscale beta n k =
  case BinInt._Z__leb 0 k of {
   Prelude.True -> (Prelude.*) n (BinInt._Z__pow (Fcore_Zaux.radix_val beta) k);
   Prelude.False ->
    BinInt._Z__quot n
      (BinInt._Z__pow (Fcore_Zaux.radix_val beta) (BinInt._Z__opp k))}

coq_Zslice :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> Prelude.Integer ->
              Prelude.Integer -> Prelude.Integer
coq_Zslice beta n k1 k2 =
  case BinInt._Z__leb 0 k2 of {
   Prelude.True ->
    BinInt._Z__rem (coq_Zscale beta n (BinInt._Z__opp k1))
      (BinInt._Z__pow (Fcore_Zaux.radix_val beta) k2);
   Prelude.False -> 0}

coq_Zdigits_aux :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> Prelude.Integer ->
                   Prelude.Integer -> Prelude.Integer -> Prelude.Integer
coq_Zdigits_aux beta p nb pow0 n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> nb)
    (\n0 ->
    case BinInt._Z__ltb p pow0 of {
     Prelude.True -> nb;
     Prelude.False ->
      coq_Zdigits_aux beta p ((Prelude.+) nb ((\x -> x) 1))
        ((Prelude.*) (Fcore_Zaux.radix_val beta) pow0) n0})
    n

coq_Zdigits :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> Prelude.Integer
coq_Zdigits beta n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p ->
    coq_Zdigits_aux beta n ((\x -> x) 1) (Fcore_Zaux.radix_val beta)
      (digits2_Pnat p))
    (\p ->
    coq_Zdigits_aux beta ((\x -> x) p) ((\x -> x) 1) (Fcore_Zaux.radix_val beta)
      (digits2_Pnat p))
    n

digits2_pos :: Prelude.Integer -> Prelude.Integer
digits2_pos n =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p -> BinPos._Pos__succ (digits2_pos p))
    (\p -> BinPos._Pos__succ (digits2_pos p))
    (\_ -> 1)
    n

coq_Zdigits2 :: Prelude.Integer -> Prelude.Integer
coq_Zdigits2 n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> n)
    (\p -> (\x -> x) (digits2_pos p))
    (\p -> (\x -> x) (digits2_pos p))
    n

