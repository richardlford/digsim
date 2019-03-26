module List0 where

import qualified Prelude
import qualified Datatypes

rev :: (([]) a1) -> ([]) a1
rev l =
  case l of {
   ([]) -> ([]);
   (:) x l' -> Datatypes.app (rev l') ((:) x ([]))}

