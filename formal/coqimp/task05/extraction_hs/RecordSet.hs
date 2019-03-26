module RecordSet where

import qualified Prelude

type Reader e t = e -> t

constructor :: a2 -> Reader a1 a2
constructor x _ =
  x

type Setter r t = (t -> t) -> r -> r
  -- singleton inductive, whose constructor was Build_Setter
  
set :: (a1 -> a2) -> (Setter a1 a2) -> (a2 -> a2) -> a1 -> a1
set _ setter =
  setter

