module Fcalc_round where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Fcalc_bracket
import qualified Fcore_Zaux
import qualified Fcore_digits

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

round_UP :: Fcalc_bracket.Coq_location -> Prelude.Bool
round_UP l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact _ -> Prelude.True}

round_sign_UP :: Prelude.Bool -> Fcalc_bracket.Coq_location -> Prelude.Bool
round_sign_UP s l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact _ -> Prelude.not s}

round_ZR :: Prelude.Bool -> Fcalc_bracket.Coq_location -> Prelude.Bool
round_ZR s l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact _ -> s}

round_N :: Prelude.Bool -> Fcalc_bracket.Coq_location -> Prelude.Bool
round_N p l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact c ->
    case c of {
     Datatypes.Eq -> p;
     Datatypes.Lt -> Prelude.False;
     Datatypes.Gt -> Prelude.True}}

truncate_aux :: Fcore_Zaux.Coq_radix -> ((,) ((,) Prelude.Integer Prelude.Integer)
                Fcalc_bracket.Coq_location) -> Prelude.Integer -> (,)
                ((,) Prelude.Integer Prelude.Integer) Fcalc_bracket.Coq_location
truncate_aux beta t k =
  case t of {
   (,) y l ->
    case y of {
     (,) m e ->
      let {p = BinInt._Z__pow (Fcore_Zaux.radix_val beta) k} in
      (,) ((,) (BinInt._Z__div m p) ((Prelude.+) e k))
      (Fcalc_bracket.new_location p (BinInt._Z__modulo m p) l)}}

truncate :: Fcore_Zaux.Coq_radix -> (Prelude.Integer -> Prelude.Integer) -> ((,)
            ((,) Prelude.Integer Prelude.Integer) Fcalc_bracket.Coq_location) -> (,)
            ((,) Prelude.Integer Prelude.Integer) Fcalc_bracket.Coq_location
truncate beta fexp t =
  case t of {
   (,) y _ ->
    case y of {
     (,) m e ->
      let {
       k = (Prelude.-) (fexp ((Prelude.+) (Fcore_digits.coq_Zdigits beta m) e)) e}
      in
      case BinInt._Z__ltb 0 k of {
       Prelude.True -> truncate_aux beta t k;
       Prelude.False -> t}}}

truncate_FIX :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> ((,)
                ((,) Prelude.Integer Prelude.Integer) Fcalc_bracket.Coq_location) ->
                (,) ((,) Prelude.Integer Prelude.Integer) Fcalc_bracket.Coq_location
truncate_FIX beta emin t =
  case t of {
   (,) y l ->
    case y of {
     (,) m e ->
      let {k = (Prelude.-) emin e} in
      case BinInt._Z__ltb 0 k of {
       Prelude.True ->
        let {p = BinInt._Z__pow (Fcore_Zaux.radix_val beta) k} in
        (,) ((,) (BinInt._Z__div m p) ((Prelude.+) e k))
        (Fcalc_bracket.new_location p (BinInt._Z__modulo m p) l);
       Prelude.False -> t}}}

