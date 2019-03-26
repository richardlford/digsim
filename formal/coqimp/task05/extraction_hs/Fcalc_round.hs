module Fcalc_round where

import qualified Prelude
import qualified Datatypes
import qualified Fcalc_bracket

cond_incr :: Prelude.Bool -> Prelude.Integer -> Prelude.Integer
cond_incr b m =
  case b of {
   Prelude.True -> (Prelude.+) m ((\x -> x) 1);
   Prelude.False -> m}

round_sign_DN :: Prelude.Bool -> Fcalc_bracket.Coq_location -> Prelude.Bool
round_sign_DN s l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact _ -> s}

round_sign_UP :: Prelude.Bool -> Fcalc_bracket.Coq_location -> Prelude.Bool
round_sign_UP s l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact _ -> Prelude.not s}

round_N :: Prelude.Bool -> Fcalc_bracket.Coq_location -> Prelude.Bool
round_N p l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact c ->
    case c of {
     Datatypes.Eq -> p;
     Datatypes.Lt -> Prelude.False;
     Datatypes.Gt -> Prelude.True}}

