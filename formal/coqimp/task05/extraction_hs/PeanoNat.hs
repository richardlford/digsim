module PeanoNat where

import qualified Prelude

_Nat__ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
_Nat__ltb n m =
  (Prelude.<=) (Prelude.succ n) m

