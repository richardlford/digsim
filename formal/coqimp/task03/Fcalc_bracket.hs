module Fcalc_bracket where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Fcore_Zaux
import qualified Zbool

data Coq_location =
   Coq_loc_Exact
 | Coq_loc_Inexact Datatypes.Coq_comparison

location_rect :: a1 -> (Datatypes.Coq_comparison -> a1) -> Coq_location -> a1
location_rect f f0 l =
  case l of {
   Coq_loc_Exact -> f;
   Coq_loc_Inexact x -> f0 x}

location_rec :: a1 -> (Datatypes.Coq_comparison -> a1) -> Coq_location -> a1
location_rec =
  location_rect

new_location_even :: Prelude.Integer -> Prelude.Integer -> Coq_location ->
                     Coq_location
new_location_even nb_steps k l =
  case Zbool.coq_Zeq_bool k 0 of {
   Prelude.True ->
    case l of {
     Coq_loc_Exact -> l;
     Coq_loc_Inexact _ -> Coq_loc_Inexact Datatypes.Lt};
   Prelude.False -> Coq_loc_Inexact
    (case BinInt._Z__compare ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) k)
            nb_steps of {
      Datatypes.Eq ->
       case l of {
        Coq_loc_Exact -> Datatypes.Eq;
        Coq_loc_Inexact _ -> Datatypes.Gt};
      x -> x})}

new_location_odd :: Prelude.Integer -> Prelude.Integer -> Coq_location ->
                    Coq_location
new_location_odd nb_steps k l =
  case Zbool.coq_Zeq_bool k 0 of {
   Prelude.True ->
    case l of {
     Coq_loc_Exact -> l;
     Coq_loc_Inexact _ -> Coq_loc_Inexact Datatypes.Lt};
   Prelude.False -> Coq_loc_Inexact
    (case BinInt._Z__compare
            ((Prelude.+) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) k)
              ((\x -> x) 1)) nb_steps of {
      Datatypes.Eq ->
       case l of {
        Coq_loc_Exact -> Datatypes.Lt;
        Coq_loc_Inexact l0 -> l0};
      x -> x})}

new_location :: Prelude.Integer -> Prelude.Integer -> Coq_location -> Coq_location
new_location nb_steps =
  case Fcore_Zaux.coq_Zeven nb_steps of {
   Prelude.True -> new_location_even nb_steps;
   Prelude.False -> new_location_odd nb_steps}

