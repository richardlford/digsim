module Coqlib where

import qualified Prelude
import qualified BinInt
import qualified ZArith_dec

zeq :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
zeq =
  BinInt._Z__eq_dec

zlt :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
zlt =
  ZArith_dec.coq_Z_lt_dec

zle :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
zle =
  ZArith_dec.coq_Z_le_gt_dec

option_map :: (a1 -> a2) -> (Prelude.Maybe a1) -> Prelude.Maybe a2
option_map f x =
  case x of {
   Prelude.Just y -> Prelude.Just (f y);
   Prelude.Nothing -> Prelude.Nothing}

