module Zbool where

import qualified Prelude
import qualified BinInt
import qualified Datatypes

coq_Zeq_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zeq_bool x y =
  case BinInt._Z__compare x y of {
   Datatypes.Eq -> Prelude.True;
   _ -> Prelude.False}

