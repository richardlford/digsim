module Digsim where

import qualified Prelude
import qualified Default_Data
import qualified InputData
import qualified State

type File = ([]) (([]) Prelude.String)

initializeState :: File -> State.State
initializeState input =
  case InputData.parser input of {
   Prelude.Left st -> st;
   Prelude.Right s ->
    State.print (State.print Default_Data.coq_Default_Data s) ((:) 'E' ((:)
      'r' ((:) 'r' ((:) 'o' ((:) 'r' ((:) 's' ((:) ' ' ((:) 'F' ((:) 'o' ((:)
      'u' ((:) 'n' ((:) 'd' ((:) ';' ((:) ' ' ((:) 'u' ((:) 's' ((:) 'i' ((:)
      'n' ((:) 'g' ((:) ' ' ((:) 'D' ((:) 'e' ((:) 'f' ((:) 'a' ((:) 'u' ((:)
      'l' ((:) 't' ((:) ' ' ((:) 'D' ((:) 'a' ((:) 't' ((:) 'a'
      ([])))))))))))))))))))))))))))))))))}

dataComments :: File -> Prelude.String
dataComments input =
  State.out (initializeState input)

