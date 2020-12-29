module Zeven where

import qualified Prelude
import qualified BinInt

coq_Zeven_odd_dec :: Prelude.Integer -> Prelude.Bool
coq_Zeven_odd_dec n =
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

coq_Zeven_dec :: Prelude.Integer -> Prelude.Bool
coq_Zeven_dec n =
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

coq_Zodd_dec :: Prelude.Integer -> Prelude.Bool
coq_Zodd_dec n =
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
    n

coq_Z_modulo_2 :: Prelude.Integer -> Prelude.Either Prelude.Integer Prelude.Integer
coq_Z_modulo_2 n =
  let {s = coq_Zeven_odd_dec n} in
  case s of {
   Prelude.True -> Prelude.Left (BinInt._Z__div2 n);
   Prelude.False -> Prelude.Right (BinInt._Z__div2 n)}

coq_Zsplit2 :: Prelude.Integer -> ((,) Prelude.Integer Prelude.Integer)
coq_Zsplit2 n =
  let {s = coq_Z_modulo_2 n} in
  case s of {
   Prelude.Left s0 -> (,) s0 s0;
   Prelude.Right s0 -> (,) s0 ((Prelude.+) s0 ((\x -> x) 1))}

