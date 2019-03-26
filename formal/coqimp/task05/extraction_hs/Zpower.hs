module Zpower where

import qualified Prelude
import qualified BinPos
import qualified Datatypes

shift_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shift_nat n z =
  Datatypes.nat_rect z (\_ x -> (\x -> 2 Prelude.* x) x) n

shift_pos :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shift_pos n z =
  BinPos._Pos__iter (\x -> (\x -> 2 Prelude.* x) x) z n

two_power_nat :: Prelude.Integer -> Prelude.Integer
two_power_nat n =
  (\x -> x) (shift_nat n 1)

