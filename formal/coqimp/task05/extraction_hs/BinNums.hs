module BinNums where

import qualified Prelude

positive_rect :: (Prelude.Integer -> a1 -> a1) -> (Prelude.Integer -> a1 -> a1) -> a1 ->
                 Prelude.Integer -> a1
positive_rect f f0 f1 p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> f p0 (positive_rect f f0 f1 p0))
    (\p0 -> f0 p0 (positive_rect f f0 f1 p0))
    (\_ -> f1)
    p

positive_rec :: (Prelude.Integer -> a1 -> a1) -> (Prelude.Integer -> a1 -> a1) -> a1 ->
                Prelude.Integer -> a1
positive_rec =
  positive_rect

data N =
   N0
 | Npos Prelude.Integer

coq_Z_rect :: a1 -> (Prelude.Integer -> a1) -> (Prelude.Integer -> a1) -> Prelude.Integer -> a1
coq_Z_rect f f0 f1 z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> f)
    (\x -> f0 x)
    (\x -> f1 x)
    z

coq_Z_rec :: a1 -> (Prelude.Integer -> a1) -> (Prelude.Integer -> a1) -> Prelude.Integer -> a1
coq_Z_rec =
  coq_Z_rect

