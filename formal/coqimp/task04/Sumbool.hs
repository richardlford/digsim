module Sumbool where

import qualified Prelude
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

sumbool_of_bool :: Prelude.Bool -> Prelude.Bool
sumbool_of_bool b =
  case b of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

bool_eq_rec :: Prelude.Bool -> (() -> a1) -> (() -> a1) -> a1
bool_eq_rec b x x0 =
  case b of {
   Prelude.True -> x __;
   Prelude.False -> x0 __}

sumbool_and :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
sumbool_and h1 h2 =
  case h1 of {
   Prelude.True -> h2;
   Prelude.False -> Prelude.False}

sumbool_or :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
sumbool_or h1 h2 =
  case h1 of {
   Prelude.True -> Prelude.True;
   Prelude.False -> h2}

sumbool_not :: Prelude.Bool -> Prelude.Bool
sumbool_not h1 =
  case h1 of {
   Prelude.True -> Prelude.False;
   Prelude.False -> Prelude.True}

bool_of_sumbool :: Prelude.Bool -> Prelude.Bool
bool_of_sumbool h =
  Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) h

