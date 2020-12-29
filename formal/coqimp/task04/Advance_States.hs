module Advance_States where

import qualified Prelude
import qualified BinPos
import qualified Differential_Equations
import qualified Events
import qualified Fin
import qualified Floats
import qualified Integers
import qualified State
import qualified String0
import qualified Termination_Conditions
import qualified Vector
import qualified Float_text_io
import qualified Data.Bits
import qualified Data.Char

advancePosition :: State.State -> Fin.Coq_t -> Floats.Coq_float ->
                   State.State
advancePosition st index dt =
  State.coq_Set_x st index
    (Floats._Float__add (Vector.nth State.coq_Ndes (State.x st) index)
      (Floats._Float__mul (Vector.nth State.coq_Ndes (State.xDot st) index)
        dt))

advanceXDot :: State.State -> Fin.Coq_t -> Floats.Coq_float -> State.State
advanceXDot st index dt =
  State.coq_Set_xDot st index
    (Floats._Float__add (Vector.nth State.coq_Ndes (State.xDot st) index)
      (Floats._Float__mul
        (Vector.nth State.coq_Ndes (Differential_Equations.coq_Xdd st) index)
        dt))

advanceIndex :: State.State -> Fin.Coq_t -> Floats.Coq_float -> State.State
advanceIndex st index dt =
  advanceXDot (advancePosition st index dt) index dt

allIndices :: Prelude.Integer -> Vector.Coq_t Fin.Coq_t
allIndices size =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Vector.Coq_nil)
    (\n -> Vector.Coq_cons (Fin.F1 n) n
    (Vector.map (\x0 -> Fin.FS n x0) n (allIndices n)))
    size

advanceIndices :: State.State -> Prelude.Integer -> (Vector.Coq_t Fin.Coq_t)
                  -> Floats.Coq_float -> State.State
advanceIndices st _ indices dt =
  case indices of {
   Vector.Coq_nil -> st;
   Vector.Coq_cons a n j -> advanceIndex (advanceIndices st n j dt) a dt}

advanceTime :: State.State -> Floats.Coq_float -> State.State
advanceTime st dt =
  State.coq_Set_Time st (Floats._Float__add (State.coq_Time st) dt)

tab :: Prelude.String
tab =
  (:) '\t' ([])

printTimePos :: State.State -> State.State
printTimePos st =
  State.print st
    (String0.append
      (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
        (Prelude.succ 0))) (State.coq_Time st))
      (String0.append tab
        (String0.append
          (Float_text_io._FloatIO__float_to_string (Prelude.succ
            (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
            (Prelude.succ (Prelude.succ 0)))
            (Vector.nth State.coq_Ndes (State.x st) (Fin.F1 (Prelude.succ
              0))))
          (String0.append tab
            (Float_text_io._FloatIO__float_to_string (Prelude.succ
              (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
              (Prelude.succ (Prelude.succ 0)))
              (Vector.nth State.coq_Ndes (State.xDot st) (Fin.F1
                (Prelude.succ 0))))))))

defaultAdvance :: State.State -> Floats.Coq_float -> State.State
defaultAdvance st dt =
  printTimePos
    (advanceTime
      (advanceIndices st State.coq_Ndes (allIndices State.coq_Ndes) dt) dt)

advance :: State.State -> State.State
advance st =
  case State.nextEvent st of {
   Prelude.Just p ->
    case p of {
     (,) advSt p0 ->
      case p0 of {
       (,) t e ->
        case e of {
         (,) cmd args ->
          case Events.executeEvent cmd args
                 (State.print advSt
                   (String0.append ((:) 'E' ((:) 'v' ((:) 'e' ((:) 'n' ((:)
                     't' ((:) ':' ((:) ' ' ([]))))))))
                     (String0.append cmd
                       (String0.append ((:) '@' ([]))
                         (Float_text_io._FloatIO__float_to_string
                           (Prelude.succ (Prelude.succ (Prelude.succ
                           (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
                           (Prelude.succ 0))) (State.coq_Time advSt)))))) of {
           Prelude.Left st' ->
            case Floats._Float__cmp Integers.Cge
                   (Floats._Float__sub (State.coq_Time st) t)
                   (State.coq_DtMin st) of {
             Prelude.True ->
              defaultAdvance st' (Floats._Float__sub (State.coq_Time st') t);
             Prelude.False -> defaultAdvance st' (State.coq_DtMin st')};
           Prelude.Right s ->
            defaultAdvance (State.print st s) (State.coq_DtMax st)}}}};
   Prelude.Nothing -> defaultAdvance st (State.coq_DtMax st)}

neededFuel :: State.State -> Prelude.Integer
neededFuel st =
  case Floats._Float__to_int
         (Floats._Float__div
           (Floats._Float__sub (State.coq_Tstop st) (State.coq_Time st))
           (State.coq_DtMin st)) of {
   Prelude.Just k ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> 0)
      (\p -> BinPos._Pos__to_nat p)
      (\_ -> 0)
      (Integers._Int__intval k);
   Prelude.Nothing -> 0}

run_fuel :: State.State -> Prelude.Integer -> State.State
run_fuel st fuel =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> st)
    (\k ->
    case Termination_Conditions.coq_Quit st of {
     Prelude.True -> st;
     Prelude.False -> run_fuel (advance st) k})
    fuel

run :: State.State -> State.State
run st =
  run_fuel st (neededFuel st)

