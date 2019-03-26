module Datatypes where

import qualified Prelude

xorb :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
xorb b1 b2 =
  case b1 of {
   Prelude.True -> case b2 of {
                    Prelude.True -> Prelude.False;
                    Prelude.False -> Prelude.True};
   Prelude.False -> b2}

nat_rect :: a1 -> (Prelude.Integer -> a1 -> a1) -> Prelude.Integer -> a1
nat_rect f f0 n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> f)
    (\n0 -> f0 n0 (nat_rect f f0 n0))
    n

app :: (([]) a1) -> (([]) a1) -> ([]) a1
app l m =
  case l of {
   ([]) -> m;
   (:) a l1 -> (:) a (app l1 m)}

data Coq_comparison =
   Eq
 | Lt
 | Gt

coq_CompOpp :: Coq_comparison -> Coq_comparison
coq_CompOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}

