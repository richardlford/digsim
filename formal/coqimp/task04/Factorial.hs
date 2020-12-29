module Factorial where

import qualified Prelude

fact :: Prelude.Integer -> Prelude.Integer
fact n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.succ 0)
    (\n0 -> (Prelude.*) (Prelude.succ n0) (fact n0))
    n

