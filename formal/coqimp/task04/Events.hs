module Events where

import qualified Prelude
import qualified Fin
import qualified Floats
import qualified List0
import qualified State
import qualified String0
import qualified Vector
import qualified Float_text_io
import qualified Data.Bits
import qualified Data.Char

getArrayField :: Prelude.String -> Fin.Coq_t -> State.State -> Prelude.Maybe
                 Prelude.String
getArrayField f i st =
  case (Prelude.==) f ((:) 'X' ([])) of {
   Prelude.True -> Prelude.Just
    (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
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
      (Prelude.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))
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
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (Vector.nth State.coq_Ndes (State.x st) i));
   Prelude.False ->
    case (Prelude.==) f ((:) 'X' ((:) 'D' ([]))) of {
     Prelude.True -> Prelude.Just
      (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
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
        (Prelude.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))
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
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (Vector.nth State.coq_Ndes (State.xDot st) i));
     Prelude.False -> Prelude.Nothing}}

getField :: Prelude.String -> State.State -> Prelude.Maybe Prelude.String
getField f st =
  case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ([]))))) of {
   Prelude.True -> Prelude.Just
    (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
      (Prelude.succ 0))) (State.coq_Time st));
   Prelude.False ->
    case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ((:) '0'
           ([])))))) of {
     Prelude.True -> Prelude.Just
      (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
        (Prelude.succ 0))) (State.coq_Time0 st));
     Prelude.False ->
      case (Prelude.==) f ((:) 'T' ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P'
             ([])))))) of {
       Prelude.True -> Prelude.Just
        (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
          (Prelude.succ 0))) (State.coq_Tstop st));
       Prelude.False ->
        case (Prelude.==) f ((:) 'D' ((:) 'T' ([]))) of {
         Prelude.True -> Prelude.Just
          (Float_text_io._FloatIO__float_to_string (Prelude.succ
            (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
            (Prelude.succ (Prelude.succ 0))) (State.coq_DtMax st));
         Prelude.False ->
          case (Prelude.==) f ((:) 'D' ((:) 'A' ((:) 'M' ((:) 'P' ((:) 'I'
                 ((:) 'N' ((:) 'G' ([])))))))) of {
           Prelude.True -> Prelude.Just
            (Float_text_io._FloatIO__float_to_string (Prelude.succ
              (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
              (Prelude.succ (Prelude.succ 0)))
              (State.coq_Damping_Coefficient st));
           Prelude.False ->
            case (Prelude.==) f ((:) 'G' ((:) 'R' ((:) 'A' ((:) 'V' ((:) 'I'
                   ((:) 'T' ((:) 'Y' ([])))))))) of {
             Prelude.True -> Prelude.Just
              (Float_text_io._FloatIO__float_to_string (Prelude.succ
                (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
                (Prelude.succ (Prelude.succ 0))) (State.coq_Gravity st));
             Prelude.False ->
              case (Prelude.==) f ((:) 'M' ((:) 'A' ((:) 'S' ((:) 'S'
                     ([]))))) of {
               Prelude.True -> Prelude.Just
                (Float_text_io._FloatIO__float_to_string (Prelude.succ
                  (Prelude.succ (Prelude.succ (Prelude.succ 0))))
                  (Prelude.succ (Prelude.succ (Prelude.succ 0)))
                  (State.coq_Mass st));
               Prelude.False ->
                case (Prelude.==) f ((:) 'S' ((:) 'P' ((:) 'R' ((:) 'I' ((:)
                       'N' ((:) 'G' ([]))))))) of {
                 Prelude.True -> Prelude.Just
                  (Float_text_io._FloatIO__float_to_string (Prelude.succ
                    (Prelude.succ (Prelude.succ (Prelude.succ 0))))
                    (Prelude.succ (Prelude.succ (Prelude.succ 0)))
                    (State.coq_Spring_Coefficient st));
                 Prelude.False -> Prelude.Nothing}}}}}}}}

stringToNatHelper :: Prelude.String -> Prelude.Maybe Prelude.Integer
stringToNatHelper s =
  case s of {
   ([]) -> Prelude.Just 0;
   (:) hd tl ->
    case stringToNatHelper tl of {
     Prelude.Just x0 ->
      (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
        (\b b0 b1 b2 b3 b4 b5 b6 ->
        case b of {
         Prelude.True ->
          case b0 of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ 0))))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ 0))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ 0))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          0))))))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing};
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ 0))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}}};
         Prelude.False ->
          case b0 of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ 0)))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ 0)))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ 0)))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0)
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ 0)))))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing};
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ 0)))))))))) x0) 0)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}}}})
        hd;
     Prelude.Nothing -> Prelude.Nothing}}

stringToList :: Prelude.String -> ([]) Prelude.Char
stringToList s =
  case s of {
   ([]) -> ([]);
   (:) hd tl -> (:) hd (stringToList tl)}

listToString :: (([]) Prelude.Char) -> Prelude.String
listToString s =
  case s of {
   ([]) -> ([]);
   (:) hd tl -> (:) hd (listToString tl)}

stringMap :: (Prelude.Char -> Prelude.Char) -> Prelude.String ->
             Prelude.String
stringMap f s =
  listToString (List0.map f (stringToList s))

reverseString :: Prelude.String -> Prelude.String
reverseString s =
  listToString (List0.rev (stringToList s))

stringToNat :: Prelude.String -> Prelude.Maybe Prelude.Integer
stringToNat s =
  case s of {
   ([]) -> Prelude.Nothing;
   (:) _ _ -> stringToNatHelper (reverseString s)}

getIndex :: Prelude.String -> Prelude.Maybe Fin.Coq_t
getIndex index =
  case stringToNat index of {
   Prelude.Just n ->
    case Fin.of_nat n State.coq_Ndes of {
     Prelude.Just x0 -> Prelude.Just x0;
     Prelude.Nothing -> Prelude.Nothing};
   Prelude.Nothing -> Prelude.Nothing}

printer :: (([]) Prelude.String) -> State.State -> Prelude.Either State.State
           Prelude.String
printer args st =
  case args of {
   ([]) -> Prelude.Right
    (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '4' ((:) ':'
      ((:) ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n'
      ((:) 'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f'
      ((:) ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n'
      ((:) 't' ((:) 's' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N'
      ((:) 'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))))))))))
      (List0.fold_right (\x0 y ->
        String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args));
   (:) f l ->
    case l of {
     ([]) ->
      case getField f st of {
       Prelude.Just x0 -> Prelude.Left (State.print st x0);
       Prelude.Nothing -> Prelude.Right
        (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '3' ((:) ':'
          ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:)
          'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm'
          ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:)
          'I' ((:) 'N' ((:) 'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))
          (List0.fold_right (\x0 y ->
            String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
     (:) index l0 ->
      case l0 of {
       ([]) ->
        case getIndex index of {
         Prelude.Just i ->
          case getArrayField f i st of {
           Prelude.Just x0 -> Prelude.Left (State.print st x0);
           Prelude.Nothing -> Prelude.Right
            (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '1' ((:)
              ':' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:)
              'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:)
              'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:)
              'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:) 'T' ((:) ' '
              ([])))))))))))))))))))))))))))))))))
              (List0.fold_right (\x0 y ->
                String0.append x0 (String0.append ((:) ' ' ([])) y)) ([])
                args))};
         Prelude.Nothing -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '2' ((:)
            ':' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:)
            'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:)
            'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:)
            'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:) 'T' ((:) ' '
            ([])))))))))))))))))))))))))))))))))
            (List0.fold_right (\x0 y ->
              String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
       (:) _ _ -> Prelude.Right
        (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '4' ((:) ':'
          ((:) ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:)
          'n' ((:) 'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o'
          ((:) 'f' ((:) ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:)
          'e' ((:) 'n' ((:) 't' ((:) 's' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R'
          ((:) 'I' ((:) 'N' ((:) 'T' ((:) ' '
          ([])))))))))))))))))))))))))))))))))))))))))
          (List0.fold_right (\x0 y ->
            String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))}}}

setArrayField :: Prelude.String -> Fin.Coq_t -> State.State -> Prelude.Maybe
                 (Floats.Coq_float -> State.State)
setArrayField f i st =
  case (Prelude.==) f ((:) 'X' ([])) of {
   Prelude.True -> Prelude.Just (State.coq_Set_x st i);
   Prelude.False ->
    case (Prelude.==) f ((:) 'X' ((:) 'D' ([]))) of {
     Prelude.True -> Prelude.Just (State.coq_Set_xDot st i);
     Prelude.False -> Prelude.Nothing}}

setField :: Prelude.String -> State.State -> Prelude.Maybe
            (Floats.Coq_float -> State.State)
setField f st =
  case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ([]))))) of {
   Prelude.True -> Prelude.Just (State.coq_Set_Time st);
   Prelude.False ->
    case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ((:) '0'
           ([])))))) of {
     Prelude.True -> Prelude.Just (State.coq_Set_Time0 st);
     Prelude.False ->
      case (Prelude.==) f ((:) 'T' ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P'
             ([])))))) of {
       Prelude.True -> Prelude.Just (State.coq_Set_Tstop st);
       Prelude.False ->
        case (Prelude.==) f ((:) 'D' ((:) 'T' ([]))) of {
         Prelude.True -> Prelude.Just (State.coq_Set_DtMax st);
         Prelude.False ->
          case (Prelude.==) f ((:) 'D' ((:) 'A' ((:) 'M' ((:) 'P' ((:) 'I'
                 ((:) 'N' ((:) 'G' ([])))))))) of {
           Prelude.True -> Prelude.Just
            (State.coq_Set_Damping_Coefficient st);
           Prelude.False ->
            case (Prelude.==) f ((:) 'G' ((:) 'R' ((:) 'A' ((:) 'V' ((:) 'I'
                   ((:) 'T' ((:) 'Y' ([])))))))) of {
             Prelude.True -> Prelude.Just (State.coq_Set_Gravity st);
             Prelude.False ->
              case (Prelude.==) f ((:) 'M' ((:) 'A' ((:) 'S' ((:) 'S'
                     ([]))))) of {
               Prelude.True -> Prelude.Just (State.coq_Set_Mass st);
               Prelude.False ->
                case (Prelude.==) f ((:) 'S' ((:) 'P' ((:) 'R' ((:) 'I' ((:)
                       'N' ((:) 'G' ([]))))))) of {
                 Prelude.True -> Prelude.Just
                  (State.coq_Set_Spring_Coefficient st);
                 Prelude.False -> Prelude.Nothing}}}}}}}}

setter :: (([]) Prelude.String) -> State.State -> Prelude.Either State.State
          Prelude.String
setter args st =
  case args of {
   ([]) -> Prelude.Right
    (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '8' ((:) ':'
      ((:) ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n'
      ((:) 'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f'
      ((:) ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n'
      ((:) 't' ((:) 's' ((:) ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
      ([])))))))))))))))))))))))))))))))))))))))
      (List0.fold_right (\x0 y ->
        String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args));
   (:) f l ->
    case l of {
     ([]) -> Prelude.Right
      (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '8' ((:) ':'
        ((:) ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:)
        'n' ((:) 'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o'
        ((:) 'f' ((:) ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:)
        'e' ((:) 'n' ((:) 't' ((:) 's' ((:) ':' ((:) ' ' ((:) 'S' ((:) 'E'
        ((:) 'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))))))))
        (List0.fold_right (\x0 y ->
          String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args));
     (:) val l0 ->
      case l0 of {
       ([]) ->
        case setField f st of {
         Prelude.Just func -> Prelude.Left
          (func (Float_text_io._FloatIO__strToFloat val));
         Prelude.Nothing -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '7' ((:)
            ':' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:)
            'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:)
            'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:)
            'S' ((:) 'E' ((:) 'T' ((:) ' ' ([])))))))))))))))))))))))))))))))
            (List0.fold_right (\x0 y ->
              String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
       (:) val0 l1 ->
        case l1 of {
         ([]) ->
          case getIndex val of {
           Prelude.Just i ->
            case setArrayField f i st of {
             Prelude.Just func -> Prelude.Left
              (func (Float_text_io._FloatIO__strToFloat val0));
             Prelude.Nothing -> Prelude.Right
              (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '5'
                ((:) ':' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:)
                'o' ((:) 'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C'
                ((:) 'o' ((:) 'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:)
                ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
                ([])))))))))))))))))))))))))))))))
                (List0.fold_right (\x0 y ->
                  String0.append x0 (String0.append ((:) ' ' ([])) y)) ([])
                  args))};
           Prelude.Nothing -> Prelude.Right
            (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '6' ((:)
              ':' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:)
              'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:)
              'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:)
              'S' ((:) 'E' ((:) 'T' ((:) ' '
              ([])))))))))))))))))))))))))))))))
              (List0.fold_right (\x0 y ->
                String0.append x0 (String0.append ((:) ' ' ([])) y)) ([])
                args))};
         (:) _ _ -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '8' ((:)
            ':' ((:) ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:)
            ' ' ((:) 'n' ((:) 'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:)
            ' ' ((:) 'o' ((:) 'f' ((:) ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:)
            'u' ((:) 'm' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:) ':' ((:)
            ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
            ([])))))))))))))))))))))))))))))))))))))))
            (List0.fold_right (\x0 y ->
              String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))}}}}

executeEvent :: Prelude.String -> (([]) Prelude.String) -> State.State ->
                Prelude.Either State.State Prelude.String
executeEvent cmd args st =
  case (Prelude.==) cmd ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:) 'T'
         ([])))))) of {
   Prelude.True -> printer args st;
   Prelude.False ->
    case (Prelude.==) cmd ((:) 'S' ((:) 'E' ((:) 'T' ([])))) of {
     Prelude.True -> setter args st;
     Prelude.False ->
      case (Prelude.==) cmd ((:) 'R' ((:) 'U' ((:) 'N' ([])))) of {
       Prelude.True -> Prelude.Right ((:) 'E' ((:) 'R' ((:) 'R' ((:) '1' ((:)
        '0' ((:) ':' ((:) ' ' ((:) 'C' ((:) 'a' ((:) 'n' ((:) 'n' ((:) 'o'
        ((:) 't' ((:) ' ' ((:) 'r' ((:) 'u' ((:) 'n' ((:) ' ' ((:) 'w' ((:)
        'h' ((:) 'i' ((:) 'l' ((:) 'e' ((:) ' ' ((:) 'a' ((:) 'l' ((:) 'r'
        ((:) 'e' ((:) 'a' ((:) 'd' ((:) 'y' ((:) ' ' ((:) 'r' ((:) 'u' ((:)
        'n' ((:) 'n' ((:) 'i' ((:) 'n' ((:) 'g' ((:) '.'
        ([])))))))))))))))))))))))))))))))))))))))));
       Prelude.False ->
        case (Prelude.==) cmd ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([]))))) of {
         Prelude.True -> Prelude.Left st;
         Prelude.False ->
          case (Prelude.==) cmd ((:) 'S' ((:) 'C' ((:) 'H' ((:) 'E' ((:) 'D'
                 ((:) 'U' ((:) 'L' ((:) 'E' ([]))))))))) of {
           Prelude.True ->
            case args of {
             ([]) -> Prelude.Right
              (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '1' ((:) '1'
                ((:) ':' ((:) ' ' ((:) 'M' ((:) 'i' ((:) 's' ((:) 'f' ((:)
                'o' ((:) 'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'S'
                ((:) 'c' ((:) 'h' ((:) 'e' ((:) 'd' ((:) 'u' ((:) 'l' ((:)
                'i' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm'
                ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' '
                ([]))))))))))))))))))))))))))))))))))))))
                (String0.append cmd
                  (String0.append ((:) ' ' ([]))
                    (List0.fold_right (\x0 y ->
                      String0.append x0 (String0.append ((:) ' ' ([])) y))
                      ([]) args))));
             (:) timestr l ->
              case l of {
               ([]) -> Prelude.Right
                (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '1' ((:) '1'
                  ((:) ':' ((:) ' ' ((:) 'M' ((:) 'i' ((:) 's' ((:) 'f' ((:)
                  'o' ((:) 'r' ((:) 'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'S'
                  ((:) 'c' ((:) 'h' ((:) 'e' ((:) 'd' ((:) 'u' ((:) 'l' ((:)
                  'i' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm'
                  ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' '
                  ([]))))))))))))))))))))))))))))))))))))))
                  (String0.append cmd
                    (String0.append ((:) ' ' ([]))
                      (List0.fold_right (\x0 y ->
                        String0.append x0 (String0.append ((:) ' ' ([])) y))
                        ([]) args))));
               (:) cmd' args' -> Prelude.Left
                (State.schedule st
                  (Float_text_io._FloatIO__strToFloat timestr) ((,) cmd'
                  args'))}};
           Prelude.False -> Prelude.Right
            (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '9' ((:) '9' ((:)
              ':' ((:) ' ' ((:) 'U' ((:) 'n' ((:) 'r' ((:) 'e' ((:) 'c' ((:)
              'o' ((:) 'g' ((:) 'n' ((:) 'i' ((:) 'z' ((:) 'e' ((:) 'd' ((:)
              ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:)
              'd' ((:) ':' ((:) ' ' ([]))))))))))))))))))))))))))))))
              (String0.append cmd
                (String0.append ((:) ' ' ([]))
                  (List0.fold_right (\x0 y ->
                    String0.append x0 (String0.append ((:) ' ' ([])) y)) ([])
                    args))))}}}}}

