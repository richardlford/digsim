module Fcore_digits where

import qualified Prelude
import qualified BinPos

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

