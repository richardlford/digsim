module ZArith_dec where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Specif

coq_Z_lt_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_lt_dec x y =
  case BinInt._Z__compare x y of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

coq_Z_le_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_le_dec x y =
  case BinInt._Z__compare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

coq_Z_le_gt_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_le_gt_dec x y =
  Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (coq_Z_le_dec x y)

