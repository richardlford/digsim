module Nat where

import qualified Prelude

pred :: Prelude.Integer -> Prelude.Integer
pred = (\n -> Prelude.max 0 (Prelude.pred n))

sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
sub = (\n m -> Prelude.max 0 (n Prelude.- m))

