module Termination_Conditions where

import qualified Prelude
import qualified Floats
import qualified Integers
import qualified State

coq_Quit :: State.State -> Prelude.Bool
coq_Quit st =
  Floats._Float__cmp Integers.Cgt (State.coq_Time st) (State.coq_Tstop st)

