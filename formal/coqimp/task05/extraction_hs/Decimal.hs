module Decimal where

import qualified Prelude

data Coq_uint =
   Nil
 | D0 Coq_uint
 | D1 Coq_uint
 | D2 Coq_uint
 | D3 Coq_uint
 | D4 Coq_uint
 | D5 Coq_uint
 | D6 Coq_uint
 | D7 Coq_uint
 | D8 Coq_uint
 | D9 Coq_uint

data Coq_int =
   Pos Coq_uint
 | Neg Coq_uint

nzhead :: Coq_uint -> Coq_uint
nzhead d =
  case d of {
   D0 d0 -> nzhead d0;
   _ -> d}

unorm :: Coq_uint -> Coq_uint
unorm d =
  case nzhead d of {
   Nil -> D0 Nil;
   x -> x}

norm :: Coq_int -> Coq_int
norm d =
  case d of {
   Pos d0 -> Pos (unorm d0);
   Neg d0 -> case nzhead d0 of {
              Nil -> Pos (D0 Nil);
              x -> Neg x}}

