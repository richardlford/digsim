module Rsqrt_def where

import qualified Prelude
import qualified Logic
import qualified RIneq
import qualified Ranalysis1
import qualified Raxioms
import qualified Rdefinitions
import qualified Rpow_def
import qualified SeqProp
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

coq_Dichotomy_lb :: Rdefinitions.R -> Rdefinitions.R -> (Rdefinitions.R ->
                    Prelude.Bool) -> Prelude.Integer -> Rdefinitions.R
coq_Dichotomy_lb x y p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> x)
    (\n0 ->
    let {down = coq_Dichotomy_lb x y p n0} in
    let {up = coq_Dichotomy_ub x y p n0} in
    let {
     z = Rdefinitions.coq_Rdiv (Rdefinitions.coq_Rplus down up)
           (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))}
    in
    case p z of {
     Prelude.True -> down;
     Prelude.False -> z})
    n

coq_Dichotomy_ub :: Rdefinitions.R -> Rdefinitions.R -> (Rdefinitions.R ->
                    Prelude.Bool) -> Prelude.Integer -> Rdefinitions.R
coq_Dichotomy_ub x y p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> y)
    (\n0 ->
    let {down = coq_Dichotomy_lb x y p n0} in
    let {up = coq_Dichotomy_ub x y p n0} in
    let {
     z = Rdefinitions.coq_Rdiv (Rdefinitions.coq_Rplus down up)
           (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))}
    in
    case p z of {
     Prelude.True -> z;
     Prelude.False -> up})
    n

dicho_lb :: Rdefinitions.R -> Rdefinitions.R -> (Rdefinitions.R ->
            Prelude.Bool) -> Prelude.Integer -> Rdefinitions.R
dicho_lb =
  coq_Dichotomy_lb

dicho_up :: Rdefinitions.R -> Rdefinitions.R -> (Rdefinitions.R ->
            Prelude.Bool) -> Prelude.Integer -> Rdefinitions.R
dicho_up =
  coq_Dichotomy_ub

dicho_lb_cv :: Rdefinitions.R -> Rdefinitions.R -> (Rdefinitions.R ->
               Prelude.Bool) -> Rdefinitions.R
dicho_lb_cv x y p =
  SeqProp.growing_cv (dicho_lb x y p)

dicho_up_cv :: Rdefinitions.R -> Rdefinitions.R -> (Rdefinitions.R ->
               Prelude.Bool) -> Rdefinitions.R
dicho_up_cv x y p =
  SeqProp.decreasing_cv (dicho_up x y p)

pow_2_n :: Prelude.Integer -> Rdefinitions.R
pow_2_n n =
  Rpow_def.pow (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1))) n

cond_positivity :: Rdefinitions.R -> Prelude.Bool
cond_positivity x =
  case RIneq.coq_Rle_dec (Rdefinitions.coq_IZR 0) x of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

coq_IVT :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R ->
           Rdefinitions.R -> Rdefinitions.R
coq_IVT f x y =
  dicho_up_cv x y (\z -> cond_positivity (f z))

coq_IVT_cor :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R ->
               Rdefinitions.R -> Rdefinitions.R
coq_IVT_cor f x y =
  let {s = Raxioms.total_order_T (Rdefinitions.coq_IZR 0) (f x)} in
  case s of {
   Prelude.Just s0 ->
    case s0 of {
     Prelude.True ->
      let {s1 = Raxioms.total_order_T (Rdefinitions.coq_IZR 0) (f y)} in
      case s1 of {
       Prelude.Just s2 ->
        case s2 of {
         Prelude.True -> Logic.coq_False_rec;
         Prelude.False -> y};
       Prelude.Nothing -> coq_IVT (Ranalysis1.opp_fct f) x y};
     Prelude.False -> x};
   Prelude.Nothing ->
    let {s0 = Raxioms.total_order_T (Rdefinitions.coq_IZR 0) (f y)} in
    case s0 of {
     Prelude.Just s1 ->
      case s1 of {
       Prelude.True -> coq_IVT f x y;
       Prelude.False -> y};
     Prelude.Nothing -> Logic.coq_False_rec}}

coq_Rsqrt_exists :: Rdefinitions.R -> Rdefinitions.R
coq_Rsqrt_exists y =
  let {f = \x -> Rdefinitions.coq_Rminus (RIneq.coq_Rsqr x) y} in
  let {s = Raxioms.total_order_T y (Rdefinitions.coq_IZR ((\x -> x) 1))} in
  case s of {
   Prelude.Just s0 ->
    case s0 of {
     Prelude.True ->
      let {
       x = coq_IVT_cor f (Rdefinitions.coq_IZR 0)
             (Rdefinitions.coq_IZR ((\x -> x) 1))}
      in
      Specif.sig_rec (\t _ -> t) x;
     Prelude.False ->
      Logic.eq_rec_r (Rdefinitions.coq_IZR ((\x -> x) 1)) (\_ _ _ ->
        Rdefinitions.coq_IZR ((\x -> x) 1)) y __ __ __};
   Prelude.Nothing ->
    let {x = coq_IVT_cor f (Rdefinitions.coq_IZR 0) y} in
    Specif.sig_rec (\t _ -> t) x}

coq_Rsqrt :: RIneq.Coq_nonnegreal -> Rdefinitions.R
coq_Rsqrt y =
  coq_Rsqrt_exists (RIneq.nonneg y)

