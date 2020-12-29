module State where

import qualified Prelude
import qualified Datatypes
import qualified Fin
import qualified Floats
import qualified Integers
import qualified String0
import qualified Vector

coq_REAL_ARRAY_SIZE :: Prelude.Integer
coq_REAL_ARRAY_SIZE =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

coq_MAX_NUMBER_OF_REAL_STATES :: Prelude.Integer
coq_MAX_NUMBER_OF_REAL_STATES =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ 0))))

coq_Ndes :: Prelude.Integer
coq_Ndes =
  Prelude.succ (Prelude.succ 0)

newline :: Prelude.String
newline =
  (:) '\r' ((:) '\n' ([]))

type Coq_event = (,) Prelude.String (([]) Prelude.String)

type EventOrder__Coq_t = (,) Floats.Coq_float Coq_event

_EventOrder__leb :: EventOrder__Coq_t -> EventOrder__Coq_t -> Prelude.Bool
_EventOrder__leb x0 y =
  case Floats._Float__cmp Integers.Cge (Prelude.fst x0) (Prelude.fst y) of {
   Prelude.True -> Prelude.True;
   Prelude.False ->
    case Floats._Float__cmp Integers.Cge (Prelude.fst y) (Prelude.fst x0) of {
     Prelude.True -> Prelude.False;
     Prelude.False -> Prelude.True}}

_EventSort__merge :: (([]) ((,) Floats.Coq_float Coq_event)) -> (([])
                     ((,) Floats.Coq_float Coq_event)) -> ([])
                     ((,) Floats.Coq_float Coq_event)
_EventSort__merge l1 l2 =
  let {
   merge_aux l3 =
     case l1 of {
      ([]) -> l3;
      (:) a1 l1' ->
       case l3 of {
        ([]) -> l1;
        (:) a2 l2' ->
         case Floats._Float__cmp Integers.Cge (Prelude.fst a1)
                (Prelude.fst a2) of {
          Prelude.True -> (:) a1 (_EventSort__merge l1' l3);
          Prelude.False ->
           case Floats._Float__cmp Integers.Cge (Prelude.fst a2)
                  (Prelude.fst a1) of {
            Prelude.True -> (:) a2 (merge_aux l2');
            Prelude.False -> (:) a1 (_EventSort__merge l1' l3)}}}}}
  in merge_aux l2

_EventSort__merge_list_to_stack :: (([])
                                   (Prelude.Maybe
                                   (([]) ((,) Floats.Coq_float Coq_event))))
                                   -> (([]) ((,) Floats.Coq_float Coq_event))
                                   -> ([])
                                   (Prelude.Maybe
                                   (([]) ((,) Floats.Coq_float Coq_event)))
_EventSort__merge_list_to_stack stack l =
  case stack of {
   ([]) -> (:) (Prelude.Just l) ([]);
   (:) y stack' ->
    case y of {
     Prelude.Just l' -> (:) Prelude.Nothing
      (_EventSort__merge_list_to_stack stack' (_EventSort__merge l' l));
     Prelude.Nothing -> (:) (Prelude.Just l) stack'}}

_EventSort__merge_stack :: (([])
                           (Prelude.Maybe
                           (([]) ((,) Floats.Coq_float Coq_event)))) -> ([])
                           ((,) Floats.Coq_float Coq_event)
_EventSort__merge_stack stack =
  case stack of {
   ([]) -> ([]);
   (:) y stack' ->
    case y of {
     Prelude.Just l -> _EventSort__merge l (_EventSort__merge_stack stack');
     Prelude.Nothing -> _EventSort__merge_stack stack'}}

_EventSort__iter_merge :: (([])
                          (Prelude.Maybe
                          (([]) ((,) Floats.Coq_float Coq_event)))) -> (([])
                          ((,) Floats.Coq_float Coq_event)) -> ([])
                          ((,) Floats.Coq_float Coq_event)
_EventSort__iter_merge stack l =
  case l of {
   ([]) -> _EventSort__merge_stack stack;
   (:) a l' ->
    _EventSort__iter_merge
      (_EventSort__merge_list_to_stack stack ((:) a ([]))) l'}

_EventSort__sort :: (([]) ((,) Floats.Coq_float Coq_event)) -> ([])
                    ((,) Floats.Coq_float Coq_event)
_EventSort__sort =
  _EventSort__iter_merge ([])

_EventSort__flatten_stack :: (([])
                             (Prelude.Maybe
                             (([]) ((,) Floats.Coq_float Coq_event)))) ->
                             ([]) ((,) Floats.Coq_float Coq_event)
_EventSort__flatten_stack stack =
  case stack of {
   ([]) -> ([]);
   (:) o stack' ->
    case o of {
     Prelude.Just l -> Datatypes.app l (_EventSort__flatten_stack stack');
     Prelude.Nothing -> _EventSort__flatten_stack stack'}}

data State =
   Coq_mkState (Vector.Coq_t Floats.Coq_float) (Vector.Coq_t
                                               Floats.Coq_float) Floats.Coq_float 
 Floats.Coq_float Floats.Coq_float Floats.Coq_float Floats.Coq_float 
 Floats.Coq_float Floats.Coq_float Floats.Coq_float Floats.Coq_float 
 Floats.Coq_float Floats.Coq_float Prelude.String (([])
                                                  ((,) Floats.Coq_float
                                                  Coq_event))

x :: State -> Vector.Coq_t Floats.Coq_float
x s =
  case s of {
   Coq_mkState x0 _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> x0}

xDot :: State -> Vector.Coq_t Floats.Coq_float
xDot s =
  case s of {
   Coq_mkState _ xDot0 _ _ _ _ _ _ _ _ _ _ _ _ _ -> xDot0}

coq_Time :: State -> Floats.Coq_float
coq_Time s =
  case s of {
   Coq_mkState _ _ time _ _ _ _ _ _ _ _ _ _ _ _ -> time}

coq_Time0 :: State -> Floats.Coq_float
coq_Time0 s =
  case s of {
   Coq_mkState _ _ _ time0 _ _ _ _ _ _ _ _ _ _ _ -> time0}

coq_Tstop :: State -> Floats.Coq_float
coq_Tstop s =
  case s of {
   Coq_mkState _ _ _ _ tstop _ _ _ _ _ _ _ _ _ _ -> tstop}

coq_DtMin :: State -> Floats.Coq_float
coq_DtMin s =
  case s of {
   Coq_mkState _ _ _ _ _ dtMin _ _ _ _ _ _ _ _ _ -> dtMin}

coq_DtMax :: State -> Floats.Coq_float
coq_DtMax s =
  case s of {
   Coq_mkState _ _ _ _ _ _ dtMax _ _ _ _ _ _ _ _ -> dtMax}

coq_Damping_Coefficient :: State -> Floats.Coq_float
coq_Damping_Coefficient s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ damping_Coefficient _ _ _ _ _ _ _ ->
    damping_Coefficient}

coq_Gravity :: State -> Floats.Coq_float
coq_Gravity s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ gravity _ _ _ _ _ _ -> gravity}

coq_Mass :: State -> Floats.Coq_float
coq_Mass s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ _ mass _ _ _ _ _ -> mass}

coq_Spring_Coefficient :: State -> Floats.Coq_float
coq_Spring_Coefficient s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ _ _ spring_Coefficient _ _ _ _ ->
    spring_Coefficient}

coq_X_IC :: State -> Floats.Coq_float
coq_X_IC s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ _ _ _ x_IC _ _ _ -> x_IC}

coq_Xd_IC :: State -> Floats.Coq_float
coq_Xd_IC s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ _ _ _ _ xd_IC _ _ -> xd_IC}

out :: State -> Prelude.String
out s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ _ _ _ _ _ out0 _ -> out0}

events :: State -> ([]) ((,) Floats.Coq_float Coq_event)
events s =
  case s of {
   Coq_mkState _ _ _ _ _ _ _ _ _ _ _ _ _ _ events0 -> events0}

coq_Set_x :: State -> Fin.Coq_t -> Floats.Coq_float -> State
coq_Set_x st index f =
  Coq_mkState (Vector.replace coq_Ndes (x st) index f) (xDot st)
    (coq_Time st) (coq_Time0 st) (coq_Tstop st) (coq_DtMin st) (coq_DtMax st)
    (coq_Damping_Coefficient st) (coq_Gravity st) (coq_Mass st)
    (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st) (out st)
    (events st)

coq_Set_xDot :: State -> Fin.Coq_t -> Floats.Coq_float -> State
coq_Set_xDot st index f =
  Coq_mkState (x st) (Vector.replace coq_Ndes (xDot st) index f)
    (coq_Time st) (coq_Time0 st) (coq_Tstop st) (coq_DtMin st) (coq_DtMax st)
    (coq_Damping_Coefficient st) (coq_Gravity st) (coq_Mass st)
    (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st) (out st)
    (events st)

eventCleaner :: Floats.Coq_float -> (([]) ((,) Floats.Coq_float Coq_event))
                -> ([]) ((,) Floats.Coq_float Coq_event)
eventCleaner time es =
  case es of {
   ([]) -> ([]);
   (:) p tl ->
    case p of {
     (,) t e ->
      case Floats._Float__cmp Integers.Cge t time of {
       Prelude.True -> (:) ((,) t e) (eventCleaner time tl);
       Prelude.False -> eventCleaner time tl}}}

cleanEvents :: State -> State
cleanEvents st =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st)
    (coq_Xd_IC st) (out st) (eventCleaner (coq_Time st) (events st))

coq_Set_Time :: State -> Floats.Coq_float -> State
coq_Set_Time st f =
  cleanEvents (Coq_mkState (x st) (xDot st) f (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st)
    (coq_Xd_IC st) (out st) (events st))

coq_Set_Time0 :: State -> Floats.Coq_float -> State
coq_Set_Time0 st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) f (coq_Tstop st) (coq_DtMin st)
    (coq_DtMax st) (coq_Damping_Coefficient st) (coq_Gravity st)
    (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st)
    (out st) (events st)

coq_Set_Tstop :: State -> Floats.Coq_float -> State
coq_Set_Tstop st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) f (coq_DtMin st)
    (coq_DtMax st) (coq_Damping_Coefficient st) (coq_Gravity st)
    (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st)
    (out st) (events st)

coq_Set_DtMin :: State -> Floats.Coq_float -> State
coq_Set_DtMin st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st) f
    (coq_DtMax st) (coq_Damping_Coefficient st) (coq_Gravity st)
    (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st)
    (out st) (events st)

coq_Set_DtMax :: State -> Floats.Coq_float -> State
coq_Set_DtMax st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMax st) f (coq_Damping_Coefficient st) (coq_Gravity st)
    (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st)
    (out st) (events st)

coq_Set_Damping_Coefficient :: State -> Floats.Coq_float -> State
coq_Set_Damping_Coefficient st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) f (coq_Gravity st) (coq_Mass st)
    (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st) (out st)
    (events st)

coq_Set_Gravity :: State -> Floats.Coq_float -> State
coq_Set_Gravity st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st) f
    (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st) (coq_Xd_IC st)
    (out st) (events st)

coq_Set_Mass :: State -> Floats.Coq_float -> State
coq_Set_Mass st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) f (coq_Spring_Coefficient st) (coq_X_IC st)
    (coq_Xd_IC st) (out st) (events st)

coq_Set_Spring_Coefficient :: State -> Floats.Coq_float -> State
coq_Set_Spring_Coefficient st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) f (coq_X_IC st) (coq_Xd_IC st) (out st)
    (events st)

coq_Set_X_IC :: State -> Floats.Coq_float -> State
coq_Set_X_IC st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) (coq_Spring_Coefficient st) f
    (coq_Xd_IC st) (out st) (events st)

coq_Set_Xd_IC :: State -> Floats.Coq_float -> State
coq_Set_Xd_IC st f =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st)
    f (out st) (events st)

print :: State -> Prelude.String -> State
print st s =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st)
    (coq_Xd_IC st)
    (case out st of {
      ([]) -> s;
      (:) a s0 -> String0.append ((:) a s0) (String0.append newline s)})
    (events st)

schedule :: State -> Floats.Coq_float -> Coq_event -> State
schedule st t e =
  Coq_mkState (x st) (xDot st) (coq_Time st) (coq_Time0 st) (coq_Tstop st)
    (coq_DtMin st) (coq_DtMax st) (coq_Damping_Coefficient st)
    (coq_Gravity st) (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st)
    (coq_Xd_IC st) (out st)
    (case Floats._Float__cmp Integers.Cge t (coq_Time st) of {
      Prelude.True -> _EventSort__sort ((:) ((,) t e) (events st));
      Prelude.False -> events st})

nextEvent :: State -> Prelude.Maybe
             ((,) State ((,) Floats.Coq_float Coq_event))
nextEvent st =
  case events st of {
   ([]) -> Prelude.Nothing;
   (:) p es ->
    case p of {
     (,) t e ->
      case (Prelude.&&)
             (Floats._Float__cmp Integers.Cge
               (Floats._Float__add (coq_Time st) (coq_DtMax st)) t)
             (Floats._Float__cmp Integers.Cge t (coq_Time st)) of {
       Prelude.True -> Prelude.Just ((,) (Coq_mkState (x st) (xDot st)
        (coq_Time st) (coq_Time0 st) (coq_Tstop st) (coq_DtMin st)
        (coq_DtMax st) (coq_Damping_Coefficient st) (coq_Gravity st)
        (coq_Mass st) (coq_Spring_Coefficient st) (coq_X_IC st)
        (coq_Xd_IC st) (out st) es) ((,) t e));
       Prelude.False -> Prelude.Nothing}}}

