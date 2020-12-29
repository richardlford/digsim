module Advance_States where

import qualified Prelude
import qualified BinPos
import qualified Differential_Equations
import qualified Fin
import qualified Floats
import qualified Integers
import qualified State
import qualified String0
import qualified Termination_Conditions
import qualified Vector
import qualified Float_text_io

advancePosition :: State.State -> Fin.Coq_t -> State.State
advancePosition st index =
  State.coq_Set_x st index
    (Floats._Float__add (Vector.nth State.coq_Ndes (State.x st) index)
      (Floats._Float__mul (Vector.nth State.coq_Ndes (State.xDot st) index)
        (State.coq_Dt st)))

advanceXDot :: State.State -> Fin.Coq_t -> State.State
advanceXDot st index =
  State.coq_Set_xDot st index
    (Floats._Float__add (Vector.nth State.coq_Ndes (State.xDot st) index)
      (Floats._Float__mul
        (Vector.nth State.coq_Ndes (Differential_Equations.coq_Xdd st) index)
        (State.coq_Dt st)))

advanceIndex :: State.State -> Fin.Coq_t -> State.State
advanceIndex st index =
  advanceXDot (advancePosition st index) index

allIndices :: Prelude.Integer -> Vector.Coq_t Fin.Coq_t
allIndices size =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Vector.Coq_nil)
    (\n -> Vector.Coq_cons (Fin.F1 n) n
    (Vector.map (\x0 -> Fin.FS n x0) n (allIndices n)))
    size

advanceIndices :: State.State -> Prelude.Integer -> (Vector.Coq_t Fin.Coq_t) ->
                  State.State
advanceIndices st _ indices =
  case indices of {
   Vector.Coq_nil -> st;
   Vector.Coq_cons a n j -> advanceIndex (advanceIndices st n j) a}

advanceTime :: State.State -> State.State
advanceTime st =
  State.coq_Set_Time st (Floats._Float__add (State.coq_Time st) (State.coq_Dt st))

advance :: State.State -> State.State
advance st =
  advanceTime (advanceIndices st State.coq_Ndes (allIndices State.coq_Ndes))

neededFuel :: State.State -> Prelude.Integer
neededFuel st =
  case Floats._Float__to_int
         (Floats._Float__div
           (Floats._Float__sub (State.coq_Tstop st) (State.coq_Time st))
           (State.coq_Dt st)) of {
   Prelude.Just k ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> 0)
      (\p -> BinPos._Pos__to_nat p)
      (\_ -> 0)
      (Integers._Int__intval k);
   Prelude.Nothing -> 0}

tab :: Prelude.String
tab =
  (:) '\t' ([])

printState :: State.State -> State.State
printState st =
  State.print st
    (String0.append
      (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ (Prelude.succ
        0))) (State.coq_Time st))
      (String0.append tab
        (String0.append
          (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
            (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
            (Prelude.succ 0)))
            (Vector.nth State.coq_Ndes (State.x st) (Fin.F1 (Prelude.succ 0))))
          (String0.append tab
            (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
              (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
              (Prelude.succ 0)))
              (Vector.nth State.coq_Ndes (State.xDot st) (Fin.F1 (Prelude.succ 0))))))))

run_fuel :: State.State -> Prelude.Integer -> State.State
run_fuel st fuel =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> printState st)
    (\_ ->
    case Termination_Conditions.coq_Quit st of {
     Prelude.True -> printState st;
     Prelude.False -> advance (printState st)})
    fuel

run :: State.State -> State.State
run st =
  run_fuel st (neededFuel st)

