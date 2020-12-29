module Plus where

import qualified Prelude
import qualified Logic

plus_is_one :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
plus_is_one m _ =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.True)
    (\m0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\_ -> Logic.coq_False_rec)
      m0)
    m

tail_plus :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
tail_plus n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> m)
    (\n0 -> tail_plus n0 (Prelude.succ m))
    n

