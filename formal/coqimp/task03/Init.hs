module Init where

import qualified Prelude

type Unconvertible a = ()

unconvertible :: a1 -> a1 -> (Unconvertible a1) -> ()
unconvertible _ _ unconvertible0 =
  unconvertible0

