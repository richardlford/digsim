module Digsim where

import qualified Prelude
import qualified Advance_States
import qualified Default_Data
import qualified Fin
import qualified Floats
import qualified InputData
import qualified State
import qualified Termination_Conditions
import qualified Vector

type File = ([]) (([]) Prelude.String)

initializeState :: File -> State.State
initializeState input =
  case InputData.parser input of {
   Prelude.Left st -> st;
   Prelude.Right s ->
    State.print (State.print Default_Data.coq_Default_Data s) ((:) 'E' ((:) 'r' ((:)
      'r' ((:) 'o' ((:) 'r' ((:) 's' ((:) ' ' ((:) 'F' ((:) 'o' ((:) 'u' ((:) 'n'
      ((:) 'd' ((:) ';' ((:) ' ' ((:) 'u' ((:) 's' ((:) 'i' ((:) 'n' ((:) 'g' ((:)
      ' ' ((:) 'D' ((:) 'e' ((:) 'f' ((:) 'a' ((:) 'u' ((:) 'l' ((:) 't' ((:) ' '
      ((:) 'D' ((:) 'a' ((:) 't' ((:) 'a' ([])))))))))))))))))))))))))))))))))}

simulateNSteps :: Prelude.Integer -> File -> State.State
simulateNSteps n input =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> initializeState input)
    (\k -> Advance_States.advance (simulateNSteps k input))
    n

graph' :: State.State -> (([]) ((,) Floats.Coq_float Floats.Coq_float)) -> Fin.Coq_t
          -> Prelude.Integer -> (,) State.State
          (([]) ((,) Floats.Coq_float Floats.Coq_float))
graph' st ps index fuel =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> (,) st ps)
    (\k ->
    case Termination_Conditions.coq_Quit st of {
     Prelude.True -> (,) st ps;
     Prelude.False ->
      graph' (Advance_States.advance st) ((:) ((,) (State.coq_Time st)
        (Vector.nth State.coq_Ndes (State.x st) index)) ps) index k})
    fuel

graph :: Fin.Coq_t -> File -> ([]) ((,) Floats.Coq_float Floats.Coq_float)
graph index input =
  Prelude.snd (graph' (initializeState input) ([]) index 251)

graphxDot' :: State.State -> (([]) ((,) Floats.Coq_float Floats.Coq_float)) ->
              Fin.Coq_t -> Prelude.Integer -> (,) State.State
              (([]) ((,) Floats.Coq_float Floats.Coq_float))
graphxDot' st ps index fuel =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> (,) st ps)
    (\k ->
    case Termination_Conditions.coq_Quit st of {
     Prelude.True -> (,) st ps;
     Prelude.False ->
      graphxDot' (Advance_States.advance st) ((:) ((,) (State.coq_Time st)
        (Vector.nth State.coq_Ndes (State.xDot st) index)) ps) index k})
    fuel

graphxDot :: Fin.Coq_t -> File -> ([]) ((,) Floats.Coq_float Floats.Coq_float)
graphxDot index input =
  Prelude.snd (graphxDot' (initializeState input) ([]) index 251)

graph1 :: File -> ([]) ((,) Floats.Coq_float Floats.Coq_float)
graph1 input =
  graph (Fin.F1 (Prelude.succ 0)) input

graph2 :: File -> ([]) ((,) Floats.Coq_float Floats.Coq_float)
graph2 input =
  graphxDot (Fin.F1 (Prelude.succ 0)) input

dataTable :: (([]) ((,) Floats.Coq_float Floats.Coq_float)) -> (([])
             ((,) Floats.Coq_float Floats.Coq_float)) -> ([])
             (([]) Floats.Coq_float)
dataTable xs xDs =
  case xs of {
   ([]) -> ([]);
   (:) p xtl ->
    case p of {
     (,) t x0 ->
      case xDs of {
       ([]) -> ([]);
       (:) p0 xdtl ->
        case p0 of {
         (,) _ xd -> (:) ((:) t ((:) x0 ((:) xd ([])))) (dataTable xtl xdtl)}}}}

graphData :: File -> ([]) (([]) Floats.Coq_float)
graphData input =
  dataTable (graph1 input) (graph2 input)

dataComments :: File -> Prelude.String
dataComments input =
  State.out
    (Prelude.fst
      (graph' (initializeState input) ([]) (Fin.F1 (Prelude.succ 0)) 251))

