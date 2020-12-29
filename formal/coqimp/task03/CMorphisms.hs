{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module CMorphisms where

import qualified Prelude
import qualified CRelationClasses
import qualified Datatypes
import qualified Init
import qualified Logic

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
unsafeCoerce :: a -> b
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
unsafeCoerce :: a -> b
unsafeCoerce = IOExts.unsafeCoerce
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

type Proper a r = r

proper_prf :: a1 -> (Proper a1 a2) -> a2
proper_prf _ proper =
  proper

type ProperProxy a r = r

proper_proxy :: a1 -> (ProperProxy a1 a2) -> a2
proper_proxy _ properProxy =
  properProxy

reflexive_proper_proxy :: (CRelationClasses.Reflexive a1 a2) -> a1 -> ProperProxy 
                          a1 a2
reflexive_proper_proxy h =
  h

proper_proper_proxy :: a1 -> (Proper a1 a2) -> ProperProxy a1 a2
proper_proper_proxy _ h =
  h

type Coq_respectful_hetero a b c d r x = a -> b -> r -> x

type Coq_respectful a b r x = a -> a -> r -> x

type Coq_forall_def a p = a -> p

type Coq_forall_relation a p sig = a -> sig

type Coq_pointwise_relation a b r = a -> r

pointwise_pointwise :: CRelationClasses.Coq_relation_equivalence (a1 -> a2)
                       (Coq_pointwise_relation a1 a2 a3)
                       (Coq_respectful a1 a2 () a3)
pointwise_pointwise _ _ =
  (,) (\x x0 y0 _ -> Logic.eq_rect_r y0 (x y0) x0) (\x a -> x a a __)

subrelation_id_proper :: (CRelationClasses.Coq_subrelation a1 a2 a3) -> Proper
                         (a1 -> a1) (Coq_respectful a1 a1 a2 a3)
subrelation_id_proper h =
  h

subrelation_respectful :: (CRelationClasses.Coq_subrelation a1 a2 a3) ->
                          (CRelationClasses.Coq_subrelation a4 a5 a6) ->
                          CRelationClasses.Coq_subrelation (a1 -> a4)
                          (Coq_respectful a1 a4 a3 a5) (Coq_respectful a1 a4 a2 a6)
subrelation_respectful subl subr x y x0 x1 y0 x2 =
  subr (x x1) (y y0) (x0 x1 y0 (subl x1 y0 x2))

subrelation_refl :: CRelationClasses.Coq_subrelation a1 a2 a2
subrelation_refl _ _ x =
  x

subrelation_proper :: a1 -> (Proper a1 a2) -> (Init.Unconvertible
                      (CRelationClasses.Coq_crelation a1)) ->
                      (CRelationClasses.Coq_subrelation a1 a2 a3) -> Proper 
                      a1 a3
subrelation_proper m mor _ sub =
  sub m m mor

proper_subrelation_proper_arrow :: (CRelationClasses.Coq_subrelation a1 a2 a3) -> a1
                                   -> a1 -> CRelationClasses.Coq_arrow
                                   (Proper a1 a2) (Proper a1 a3)
proper_subrelation_proper_arrow x x0 y0 x1 =
  Logic.eq_rect_r y0 (\x2 -> x y0 y0 x2) x0 x1

pointwise_subrelation :: (CRelationClasses.Coq_subrelation a2 a3 a4) ->
                         CRelationClasses.Coq_subrelation (a1 -> a2)
                         (Coq_pointwise_relation a1 a2 a3)
                         (Coq_pointwise_relation a1 a2 a4)
pointwise_subrelation sub x y x0 a =
  sub (x a) (y a) (x0 a)

forall_subrelation :: (a1 -> CRelationClasses.Coq_subrelation a2 a3 a4) ->
                      CRelationClasses.Coq_subrelation (a1 -> a2)
                      (Coq_forall_relation a1 a2 a3) (Coq_forall_relation a1 a2 a4)
forall_subrelation x x0 y x1 a =
  let {x2 = x a} in let {x3 = x1 a} in x2 (x0 a) (y a) x3

iffT_arrow_subrelation :: (CRelationClasses.Coq_iffT a1 a2) ->
                          CRelationClasses.Coq_arrow a1 a2
iffT_arrow_subrelation x x0 =
  Datatypes.prod_rect (\a _ -> a x0) x

iffT_flip_arrow_subrelation :: (CRelationClasses.Coq_iffT a1 a2) ->
                               CRelationClasses.Coq_arrow a2 a1
iffT_flip_arrow_subrelation x x0 =
  Datatypes.prod_rect (\_ b -> b x0) x

respectful_per_obligation_1 :: (CRelationClasses.PER a1 a2) -> (CRelationClasses.PER
                               a3 a4) -> CRelationClasses.Symmetric (a1 -> a3)
                               (Coq_respectful a1 a3 a2 a4)
respectful_per_obligation_1 h h0 x y x0 x1 y0 x2 =
  case h of {
   CRelationClasses.Build_PER pER_Symmetric _ ->
    case h0 of {
     CRelationClasses.Build_PER pER_Symmetric0 _ ->
      pER_Symmetric0 (x y0) (y x1) (x0 y0 x1 (pER_Symmetric x1 y0 x2))}}

respectful_per_obligation_2 :: (CRelationClasses.PER a1 a2) -> (CRelationClasses.PER
                               a3 a4) -> CRelationClasses.Transitive (a1 -> a3)
                               (Coq_respectful a1 a3 a2 a4)
respectful_per_obligation_2 h h0 x y z x0 x1 x2 y0 x3 =
  let {
   x4 = CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) x2 y0
          x2 x3
          (CRelationClasses.symmetry (CRelationClasses.coq_PER_Symmetric h) x2 y0
            x3)}
  in
  CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h0) (x x2)
    (y x2) (z y0) (x0 x2 x2 x4) (x1 x2 y0 x3)

respectful_per :: (CRelationClasses.PER a1 a2) -> (CRelationClasses.PER a3 a4) ->
                  CRelationClasses.PER (a1 -> a3) (Coq_respectful a1 a3 a2 a4)
respectful_per h h0 =
  CRelationClasses.Build_PER (respectful_per_obligation_1 h h0)
    (respectful_per_obligation_2 h h0)

flip_proper_obligation_1 :: (a1 -> a2 -> a3) -> (Proper (a1 -> a2 -> a3)
                            (Coq_respectful a1 (a2 -> a3) a4
                            (Coq_respectful a2 a3 a5 a6))) -> Proper
                            (a2 -> a1 -> a3)
                            (Coq_respectful a2 (a1 -> a3) a5
                            (Coq_respectful a1 a3 a4 a6))
flip_proper_obligation_1 _ mor x y x0 x1 y0 x2 =
  mor x1 y0 x2 x y x0

flip_proper :: (a1 -> a2 -> a3) -> (Proper (a1 -> a2 -> a3)
               (Coq_respectful a1 (a2 -> a3) a4 (Coq_respectful a2 a3 a5 a6))) ->
               Proper (a2 -> a1 -> a3)
               (Coq_respectful a2 (a1 -> a3) a5 (Coq_respectful a1 a3 a4 a6))
flip_proper =
  flip_proper_obligation_1

trans_contra_co_type_morphism_obligation_1 :: (CRelationClasses.Transitive a1 
                                              a2) -> a1 -> a1 -> a2 -> a1 -> a1 ->
                                              a2 -> CRelationClasses.Coq_arrow 
                                              a2 a2
trans_contra_co_type_morphism_obligation_1 h x y x0 x1 y0 x2 x3 =
  CRelationClasses.transitivity h y x y0 x0
    (CRelationClasses.transitivity h x x1 y0 x3 x2)

trans_contra_co_type_morphism :: (CRelationClasses.Transitive a1 a2) -> a1 -> a1 ->
                                 a2 -> a1 -> a1 -> a2 -> CRelationClasses.Coq_arrow
                                 a2 a2
trans_contra_co_type_morphism =
  trans_contra_co_type_morphism_obligation_1

trans_contra_inv_impl_type_morphism_obligation_1 :: (CRelationClasses.Transitive 
                                                    a1 a2) -> a1 -> a1 -> a1 -> a2
                                                    -> CRelationClasses.Coq_arrow 
                                                    a2 a2
trans_contra_inv_impl_type_morphism_obligation_1 h x x0 y x1 x2 =
  CRelationClasses.transitivity h x y x0 x2 x1

trans_contra_inv_impl_type_morphism :: (CRelationClasses.Transitive a1 a2) -> a1 ->
                                       a1 -> a1 -> a2 -> CRelationClasses.Coq_arrow
                                       a2 a2
trans_contra_inv_impl_type_morphism =
  trans_contra_inv_impl_type_morphism_obligation_1

trans_co_impl_type_morphism_obligation_1 :: (CRelationClasses.Transitive a1 
                                            a2) -> a1 -> a1 -> a1 -> a2 ->
                                            CRelationClasses.Coq_arrow a2 a2
trans_co_impl_type_morphism_obligation_1 h x x0 y x1 x2 =
  CRelationClasses.transitivity h x x0 y x2 x1

trans_co_impl_type_morphism :: (CRelationClasses.Transitive a1 a2) -> a1 -> a1 -> a1
                               -> a2 -> CRelationClasses.Coq_arrow a2 a2
trans_co_impl_type_morphism =
  trans_co_impl_type_morphism_obligation_1

trans_sym_co_inv_impl_type_morphism_obligation_1 :: (CRelationClasses.PER a1 
                                                    a2) -> a1 -> a1 -> a1 -> a2 ->
                                                    CRelationClasses.Coq_arrow 
                                                    a2 a2
trans_sym_co_inv_impl_type_morphism_obligation_1 h x x0 y x1 x2 =
  CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) x y x0 x2
    (CRelationClasses.symmetry (CRelationClasses.coq_PER_Symmetric h) x0 y x1)

trans_sym_co_inv_impl_type_morphism :: (CRelationClasses.PER a1 a2) -> a1 -> a1 ->
                                       a1 -> a2 -> CRelationClasses.Coq_arrow 
                                       a2 a2
trans_sym_co_inv_impl_type_morphism =
  trans_sym_co_inv_impl_type_morphism_obligation_1

trans_sym_contra_arrow_morphism_obligation_1 :: (CRelationClasses.PER a1 a2) -> a1
                                                -> a1 -> a1 -> a2 ->
                                                CRelationClasses.Coq_arrow a2 
                                                a2
trans_sym_contra_arrow_morphism_obligation_1 h x x0 y x1 x2 =
  CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) x x0 y x2
    (CRelationClasses.symmetry (CRelationClasses.coq_PER_Symmetric h) y x0 x1)

trans_sym_contra_arrow_morphism :: (CRelationClasses.PER a1 a2) -> a1 -> a1 -> a1 ->
                                   a2 -> CRelationClasses.Coq_arrow a2 a2
trans_sym_contra_arrow_morphism =
  trans_sym_contra_arrow_morphism_obligation_1

per_partial_app_type_morphism_obligation_1 :: (CRelationClasses.PER a1 a2) -> a1 ->
                                              a1 -> a1 -> a2 ->
                                              CRelationClasses.Coq_iffT a2 a2
per_partial_app_type_morphism_obligation_1 h x x0 y x1 =
  (,) (\x2 ->
    CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) x x0 y x2
      x1) (\x2 ->
    CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) x y x0 x2
      (CRelationClasses.symmetry (CRelationClasses.coq_PER_Symmetric h) x0 y x1))

per_partial_app_type_morphism :: (CRelationClasses.PER a1 a2) -> a1 -> a1 -> a1 ->
                                 a2 -> CRelationClasses.Coq_iffT a2 a2
per_partial_app_type_morphism =
  per_partial_app_type_morphism_obligation_1

trans_co_eq_inv_arrow_morphism_obligation_1 :: (CRelationClasses.Transitive 
                                               a1 a2) -> a1 -> a1 -> a2 -> a1 -> a1
                                               -> CRelationClasses.Coq_arrow 
                                               a2 a2
trans_co_eq_inv_arrow_morphism_obligation_1 h x y x0 x1 y0 x2 =
  Logic.eq_rect_r y0 (CRelationClasses.transitivity h x y y0 x0 x2) x1

trans_co_eq_inv_arrow_morphism :: (CRelationClasses.Transitive a1 a2) -> a1 -> a1 ->
                                  a2 -> a1 -> a1 -> CRelationClasses.Coq_arrow 
                                  a2 a2
trans_co_eq_inv_arrow_morphism =
  trans_co_eq_inv_arrow_morphism_obligation_1

coq_PER_type_morphism_obligation_1 :: (CRelationClasses.PER a1 a2) -> a1 -> a1 -> a2
                                      -> a1 -> a1 -> a2 -> CRelationClasses.Coq_iffT
                                      a2 a2
coq_PER_type_morphism_obligation_1 h x y x0 x1 y0 x2 =
  (,) (\x3 ->
    CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) y x1 y0
      (CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) y x x1
        (CRelationClasses.symmetry (CRelationClasses.coq_PER_Symmetric h) x y x0)
        x3) x2) (\x3 ->
    CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) x y x1 x0
      (CRelationClasses.transitivity (CRelationClasses.coq_PER_Transitive h) y y0 x1
        x3
        (CRelationClasses.symmetry (CRelationClasses.coq_PER_Symmetric h) x1 y0 x2)))

coq_PER_type_morphism :: (CRelationClasses.PER a1 a2) -> a1 -> a1 -> a2 -> a1 -> a1
                         -> a2 -> CRelationClasses.Coq_iffT a2 a2
coq_PER_type_morphism =
  coq_PER_type_morphism_obligation_1

symmetric_equiv_flip :: (CRelationClasses.Symmetric a1 a2) ->
                        CRelationClasses.Coq_relation_equivalence a1 a2 a2
symmetric_equiv_flip h x y =
  (,) (\x0 -> h x y x0) (\x0 -> h y x x0)

compose_proper_obligation_1 :: Coq_respectful (a2 -> a3) ((a1 -> a2) -> a1 -> a3)
                               (Coq_respectful a2 a3 a5 a6)
                               (Coq_respectful (a1 -> a2) (a1 -> a3)
                               (Coq_respectful a1 a2 a4 a5)
                               (Coq_respectful a1 a3 a4 a6))
compose_proper_obligation_1 _ _ x x0 y0 x1 x2 y1 x3 =
  x (x0 x2) (y0 y1) (x1 x2 y1 x3)

compose_proper :: Proper ((a2 -> a3) -> (a1 -> a2) -> a1 -> a3)
                  (Coq_respectful (a2 -> a3) ((a1 -> a2) -> a1 -> a3)
                  (Coq_respectful a2 a3 a5 a6)
                  (Coq_respectful (a1 -> a2) (a1 -> a3) (Coq_respectful a1 a2 a4 a5)
                  (Coq_respectful a1 a3 a4 a6)))
compose_proper =
  compose_proper_obligation_1

reflexive_eq_dom_reflexive :: (CRelationClasses.Reflexive a1 a2) -> (a3 -> a1) -> a3
                              -> a3 -> a2
reflexive_eq_dom_reflexive h x x0 y =
  Logic.eq_rect_r y (h (x y)) x0

respectful_morphism :: (CRelationClasses.Coq_relation_equivalence a1 a3 a4) ->
                       (CRelationClasses.Coq_relation_equivalence a2 a5 a6) ->
                       CRelationClasses.Coq_relation_equivalence (a1 -> a2)
                       (Coq_respectful a1 a2 a3 a5) (Coq_respectful a1 a2 a4 a6)
respectful_morphism hRR' hSS' f g =
  (,) (\h x y hxy ->
    Prelude.fst (hSS' (f x) (g y)) (h x y (Prelude.snd (hRR' x y) hxy)))
    (\h x y hxy ->
    Prelude.snd (hSS' (f x) (g y)) (h x y (Prelude.fst (hRR' x y) hxy)))

coq_Reflexive_partial_app_morphism :: (a1 -> a2) -> (Proper (a1 -> a2)
                                      (Coq_respectful a1 a2 a3 a4)) -> a1 ->
                                      (ProperProxy a1 a3) -> Proper a2 a4
coq_Reflexive_partial_app_morphism _ h x h0 =
  h x x h0

flip_respectful :: CRelationClasses.Coq_relation_equivalence (a1 -> a2)
                   (Coq_respectful a1 a2 a3 a4) (Coq_respectful a1 a2 a3 a4)
flip_respectful _ _ =
  (,) (\x x0 y0 x1 -> x y0 x0 x1) (\x x0 y0 x1 -> x y0 x0 x1)

flip1 :: (CRelationClasses.Coq_subrelation a1 a2 a3) ->
         CRelationClasses.Coq_subrelation a1 a2 a3
flip1 h =
  h

flip2 :: (CRelationClasses.Coq_subrelation a1 a2 a3) ->
         CRelationClasses.Coq_subrelation a1 a2 a3
flip2 h =
  h

eq_subrelation :: (CRelationClasses.Reflexive a1 a2) -> a1 -> a1 -> a2
eq_subrelation h x y =
  Logic.eq_rect_r y (h y) x

proper_flip_proper :: a1 -> (Proper a1 a2) -> Proper a1 a2
proper_flip_proper _ mor =
  mor

reflexive_proper :: (CRelationClasses.Reflexive a1 a2) -> a1 -> Proper a1 a2
reflexive_proper h =
  h

proper_proper :: (CRelationClasses.Coq_relation_equivalence a1 a2 a3) -> a1 -> a1 ->
                 CRelationClasses.Coq_iffT (Proper a1 a2) (Proper a1 a3)
proper_proper hRR' x y =
  Logic.eq_rect x ((,) (\x0 -> Prelude.fst (hRR' x x) x0) (\x0 ->
    Prelude.snd (hRR' x x) x0)) y

type Normalizes a m x = CRelationClasses.Coq_relation_equivalence a m x

normalizes :: (Normalizes a1 a2 a3) -> CRelationClasses.Coq_relation_equivalence 
              a1 a2 a3
normalizes normalizes0 =
  normalizes0

proper_normalizes_proper :: (Normalizes a1 a2 a3) -> a1 -> (Proper a1 a3) -> Proper
                            a1 a2
proper_normalizes_proper h m h0 =
  Prelude.snd (h m m) h0

flip_atom :: Normalizes a1 a2 a2
flip_atom _ _ =
  (,) (\x -> x) (\x -> x)

flip_arrow :: (Normalizes a1 a2 a3) -> (Normalizes a4 a5 a6) -> Normalizes
              (a1 -> a4) (Coq_respectful a1 a4 a2 a5) (Coq_respectful a1 a4 a3 a6)
flip_arrow nA nB =
  trans_co_eq_inv_arrow_morphism
    (CRelationClasses.coq_Equivalence_Transitive
      (unsafeCoerce CRelationClasses.relation_equivalence_equivalence)) __ __
    (respectful_morphism nA
      (reflexive_proper_proxy
        (CRelationClasses.coq_Equivalence_Reflexive
          (unsafeCoerce CRelationClasses.relation_equivalence_equivalence)) __)) __
    __
    (trans_co_eq_inv_arrow_morphism
      (CRelationClasses.coq_Equivalence_Transitive
        (unsafeCoerce CRelationClasses.relation_equivalence_equivalence)) __ __
      (coq_Reflexive_partial_app_morphism __
        (unsafeCoerce (\_ _ x _ _ -> respectful_morphism x)) __
        (reflexive_proper_proxy
          (CRelationClasses.coq_Equivalence_Reflexive
            CRelationClasses.relation_equivalence_equivalence) __) __ __ nB) __ __
      (\_ _ -> (,) (\x x0 y0 x1 ->
      let {x2 = x y0} in let {x3 = x2 x0} in unsafeCoerce x3 x1) (\x x0 y0 x1 ->
      let {x2 = x y0} in let {x3 = x2 x0} in unsafeCoerce x3 x1)))

proper_sym_flip :: (CRelationClasses.Symmetric a1 a2) -> (a1 -> a3) -> (Proper
                   (a1 -> a3) (Coq_respectful a1 a3 a2 a4)) -> Proper (a1 -> a3)
                   (Coq_respectful a1 a3 a2 a4)
proper_sym_flip sym _ hf x x' hxx' =
  hf x' x (sym x x' hxx')

proper_sym_flip_2 :: (CRelationClasses.Symmetric a1 a2) ->
                     (CRelationClasses.Symmetric a3 a4) -> (a1 -> a3 -> a5) ->
                     (Proper (a1 -> a3 -> a5)
                     (Coq_respectful a1 (a3 -> a5) a2 (Coq_respectful a3 a5 a4 a6)))
                     -> Proper (a1 -> a3 -> a5)
                     (Coq_respectful a1 (a3 -> a5) a2 (Coq_respectful a3 a5 a4 a6))
proper_sym_flip_2 sym1 sym2 _ hf x x' hxx' y y' hyy' =
  hf x' x (sym1 x x' hxx') y' y (sym2 y y' hyy')

proper_sym_arrow_iffT :: (CRelationClasses.Symmetric a1 a2) -> (a1 -> a1 -> a2 ->
                         CRelationClasses.Coq_arrow a3 a3) -> a1 -> a1 -> a2 ->
                         CRelationClasses.Coq_iffT a3 a3
proper_sym_arrow_iffT sym hf x x' hxx' =
  (,) (\x0 -> hf x x' hxx' x0) (\x0 -> hf x' x (sym x x' hxx') x0)

proper_sym_arrow_iffT_2 :: (CRelationClasses.Symmetric a1 a2) ->
                           (CRelationClasses.Symmetric a3 a4) -> (a1 -> a1 -> a2 ->
                           a3 -> a3 -> a4 -> CRelationClasses.Coq_arrow a5 a5) -> a1
                           -> a1 -> a2 -> a3 -> a3 -> a4 ->
                           CRelationClasses.Coq_iffT a5 a5
proper_sym_arrow_iffT_2 sym sym' hf x x' hxx' y y' hyy' =
  (,) (\x0 -> hf x x' hxx' y y' hyy' x0) (\x0 ->
    hf x' x (sym x x' hxx') y' y (sym' y y' hyy') x0)

coq_PartialOrder_proper_type :: (CRelationClasses.Equivalence a1 a2) ->
                                (CRelationClasses.PreOrder a1 a3) ->
                                (CRelationClasses.PartialOrder a1 a2 a3) -> a1 -> a1
                                -> a2 -> a1 -> a1 -> a2 -> CRelationClasses.Coq_iffT
                                a3 a3
coq_PartialOrder_proper_type equ preo h =
  proper_sym_arrow_iffT_2 (\x y x0 ->
    CRelationClasses.symmetry (CRelationClasses.coq_Equivalence_Symmetric equ) x y
      x0) (\x y x0 ->
    CRelationClasses.symmetry (CRelationClasses.coq_Equivalence_Symmetric equ) x y
      x0) (\x x' hx y y' hy hr ->
    CRelationClasses.transitivity (CRelationClasses.coq_PreOrder_Transitive preo) x'
      x y'
      (let {x0 = CRelationClasses.partial_order_equivalence equ preo h x x'} in
       Datatypes.prod_rect (\a _ ->
         let {x1 = a hx} in Datatypes.prod_rect (\_ b0 -> b0) x1) x0)
      (CRelationClasses.transitivity (CRelationClasses.coq_PreOrder_Transitive preo)
        x y y' hr
        (let {x0 = CRelationClasses.partial_order_equivalence equ preo h y y'} in
         Datatypes.prod_rect (\a _ ->
           let {x1 = a hy} in Datatypes.prod_rect (\a0 _ -> a0) x1) x0)))

coq_PartialOrder_StrictOrder :: (CRelationClasses.Equivalence a1 a2) ->
                                (CRelationClasses.PreOrder a1 a3) ->
                                (CRelationClasses.PartialOrder a1 a2 a3) ->
                                CRelationClasses.StrictOrder a1
                                (CRelationClasses.Coq_relation_conjunction a1 
                                a3 (CRelationClasses.Coq_complement a1 a2))
coq_PartialOrder_StrictOrder _ preo _ x y z x0 x1 =
  case x0 of {
   (,) hxy _ ->
    case x1 of {
     (,) hyz _ -> (,) (CRelationClasses.coq_PreOrder_Transitive preo x y z hxy hyz)
      __}}

coq_StrictOrder_PreOrder :: (CRelationClasses.Equivalence a1 a2) ->
                            (CRelationClasses.StrictOrder a1 a3) -> (a1 -> a1 -> a2
                            -> a1 -> a1 -> a2 -> CRelationClasses.Coq_iffT a3 
                            a3) -> CRelationClasses.PreOrder a1
                            (CRelationClasses.Coq_relation_disjunction a1 a3 a2)
coq_StrictOrder_PreOrder h h0 h1 =
  CRelationClasses.Build_PreOrder (\x -> Prelude.Right
    (CRelationClasses.reflexivity (CRelationClasses.coq_Equivalence_Reflexive h) x))
    (\x y z x0 x1 ->
    case x0 of {
     Prelude.Left hxy ->
      case x1 of {
       Prelude.Left hyz -> Prelude.Left
        (CRelationClasses.transitivity
          (CRelationClasses.coq_StrictOrder_Transitive h0) x y z hxy hyz);
       Prelude.Right hyz -> Prelude.Left
        (coq_Reflexive_partial_app_morphism __
          (subrelation_proper __ h1 ()
            (subrelation_respectful subrelation_refl
              (subrelation_respectful subrelation_refl
                (flip2 (\_ _ -> iffT_flip_arrow_subrelation))))) x
          (reflexive_proper_proxy (CRelationClasses.coq_Equivalence_Reflexive h) x)
          z y
          (CRelationClasses.symmetry (CRelationClasses.coq_Equivalence_Symmetric h)
            y z hyz) hxy)};
     Prelude.Right hxy ->
      case x1 of {
       Prelude.Left hyz -> Prelude.Left
        (subrelation_proper __ h1 ()
          (subrelation_respectful subrelation_refl
            (subrelation_respectful subrelation_refl
              (flip2 (\_ _ -> iffT_flip_arrow_subrelation)))) x y hxy z z
          (reflexive_proper_proxy (CRelationClasses.coq_Equivalence_Reflexive h) z)
          hyz);
       Prelude.Right hyz -> Prelude.Right
        (CRelationClasses.transitivity
          (CRelationClasses.coq_Equivalence_Transitive h) x y z hxy hyz)}})

coq_StrictOrder_PartialOrder :: (CRelationClasses.Equivalence a1 a2) ->
                                (CRelationClasses.StrictOrder a1 a3) -> (a1 -> a1 ->
                                a2 -> a1 -> a1 -> a2 -> CRelationClasses.Coq_iffT 
                                a3 a3) -> CRelationClasses.PartialOrder a1 a2
                                (CRelationClasses.Coq_relation_disjunction a1 a3 a2)
coq_StrictOrder_PartialOrder h _ _ x y =
  (,) (\x0 -> (,) (Prelude.Right x0) (Prelude.Right
    (CRelationClasses.symmetry (CRelationClasses.coq_Equivalence_Symmetric h) x y
      x0))) (\x0 ->
    Datatypes.prod_rect (\a b ->
      Datatypes.sum_rect (\_ ->
        Datatypes.sum_rect (\_ -> Logic.coq_False_rect) (\b0 ->
          CRelationClasses.symmetry (CRelationClasses.coq_Equivalence_Symmetric h) y
            x b0) b) (\b0 -> Datatypes.sum_rect (\_ -> b0) (\_ -> b0) b) a) x0)

