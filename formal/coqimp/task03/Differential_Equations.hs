module Differential_Equations where

import qualified Prelude
import qualified Floats
import qualified State
import qualified Vector

coq_Xdd :: State.State -> Vector.Coq_t Floats.Coq_float
coq_Xdd st =
  Vector.map2 (\x0 xd ->
    Floats._Float__sub
      (Floats._Float__div
        (Floats._Float__neg
          (Floats._Float__add
            (Floats._Float__mul (State.coq_Spring_Coefficient st) x0)
            (Floats._Float__mul (State.coq_Damping_Coefficient st) xd)))
        (State.coq_Mass st)) (State.coq_Gravity st)) State.coq_Ndes (State.x st)
    (State.xDot st)

