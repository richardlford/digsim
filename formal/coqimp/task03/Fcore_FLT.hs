module Fcore_FLT where

import qualified Prelude

coq_FLT_exp :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
               Prelude.Integer
coq_FLT_exp emin prec e =
  Prelude.max ((Prelude.-) e prec) emin

