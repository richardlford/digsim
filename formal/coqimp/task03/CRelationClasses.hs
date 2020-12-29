{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module CRelationClasses where

import qualified Prelude
import qualified Datatypes

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
type Any = GHC.Base.Any
#else
-- HUGS
type Any = ()
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

type Coq_crelation a = ()

type Coq_arrow a b = a -> b

flip :: (a1 -> a2 -> a3) -> a2 -> a1 -> a3
flip f x y =
  f y x

type Coq_iffT a b = (,) (a -> b) (b -> a)

type Reflexive a r = a -> r

reflexivity :: (Reflexive a1 a2) -> a1 -> a2
reflexivity reflexive =
  reflexive

type Coq_complement a r = ()

type Irreflexive a r = ()

type Symmetric a r = a -> a -> r -> r

symmetry :: (Symmetric a1 a2) -> a1 -> a1 -> a2 -> a2
symmetry symmetric =
  symmetric

type Asymmetric a r = ()

type Transitive a r = a -> a -> a -> r -> r -> r

transitivity :: (Transitive a1 a2) -> a1 -> a1 -> a1 -> a2 -> a2 -> a2
transitivity transitive =
  transitive

data PreOrder a r =
   Build_PreOrder (Reflexive a r) (Transitive a r)

coq_PreOrder_Reflexive :: (PreOrder a1 a2) -> Reflexive a1 a2
coq_PreOrder_Reflexive preOrder =
  case preOrder of {
   Build_PreOrder preOrder_Reflexive _ -> preOrder_Reflexive}

coq_PreOrder_Transitive :: (PreOrder a1 a2) -> Transitive a1 a2
coq_PreOrder_Transitive preOrder =
  case preOrder of {
   Build_PreOrder _ preOrder_Transitive -> preOrder_Transitive}

type StrictOrder a r =
  Transitive a r
  -- singleton inductive, whose constructor was Build_StrictOrder
  
coq_StrictOrder_Transitive :: (StrictOrder a1 a2) -> Transitive a1 a2
coq_StrictOrder_Transitive strictOrder =
  strictOrder

data PER a r =
   Build_PER (Symmetric a r) (Transitive a r)

coq_PER_Symmetric :: (PER a1 a2) -> Symmetric a1 a2
coq_PER_Symmetric pER =
  case pER of {
   Build_PER pER_Symmetric _ -> pER_Symmetric}

coq_PER_Transitive :: (PER a1 a2) -> Transitive a1 a2
coq_PER_Transitive pER =
  case pER of {
   Build_PER _ pER_Transitive -> pER_Transitive}

data Equivalence a r =
   Build_Equivalence (Reflexive a r) (Symmetric a r) (Transitive a r)

coq_Equivalence_Reflexive :: (Equivalence a1 a2) -> Reflexive a1 a2
coq_Equivalence_Reflexive equivalence =
  case equivalence of {
   Build_Equivalence equivalence_Reflexive _ _ -> equivalence_Reflexive}

coq_Equivalence_Symmetric :: (Equivalence a1 a2) -> Symmetric a1 a2
coq_Equivalence_Symmetric equivalence =
  case equivalence of {
   Build_Equivalence _ equivalence_Symmetric _ -> equivalence_Symmetric}

coq_Equivalence_Transitive :: (Equivalence a1 a2) -> Transitive a1 a2
coq_Equivalence_Transitive equivalence =
  case equivalence of {
   Build_Equivalence _ _ equivalence_Transitive -> equivalence_Transitive}

coq_Equivalence_PER :: (Equivalence a1 a2) -> PER a1 a2
coq_Equivalence_PER h =
  Build_PER (coq_Equivalence_Symmetric h) (coq_Equivalence_Transitive h)

type Antisymmetric a eqA r = a -> a -> r -> r -> eqA

antisymmetry :: (Equivalence a1 a2) -> (Antisymmetric a1 a2 a3) -> a1 -> a1 -> a3 ->
                a3 -> a2
antisymmetry _ antisymmetric =
  antisymmetric

type Coq_subrelation a r x = a -> a -> r -> x

is_subrelation :: (Coq_subrelation a1 a2 a3) -> a1 -> a1 -> a2 -> a3
is_subrelation subrelation =
  subrelation

subrelation_symmetric :: (Symmetric a1 a2) -> Coq_subrelation a1 a2 a2
subrelation_symmetric h x y h' =
  symmetry h y x h'

flip_Reflexive :: (Reflexive a1 a2) -> Reflexive a1 a2
flip_Reflexive h =
  h

flip_Symmetric :: (Symmetric a1 a2) -> Symmetric a1 a2
flip_Symmetric h x y h0 =
  symmetry h y x h0

flip_Transitive :: (Transitive a1 a2) -> Transitive a1 a2
flip_Transitive h x y z h0 h' =
  transitivity h z y x h' h0

flip_Antisymmetric :: (Equivalence a1 a2) -> (Antisymmetric a1 a2 a3) ->
                      Antisymmetric a1 a2 a3
flip_Antisymmetric _ h x y x0 x1 =
  h x y x1 x0

flip_PreOrder :: (PreOrder a1 a2) -> PreOrder a1 a2
flip_PreOrder h =
  case h of {
   Build_PreOrder preOrder_Reflexive0 preOrder_Transitive0 -> Build_PreOrder
    preOrder_Reflexive0 (\x y z x0 x1 -> preOrder_Transitive0 z y x x1 x0)}

flip_StrictOrder :: (StrictOrder a1 a2) -> StrictOrder a1 a2
flip_StrictOrder h x y z x0 x1 =
  h z y x x1 x0

flip_PER :: (PER a1 a2) -> PER a1 a2
flip_PER h =
  case h of {
   Build_PER pER_Symmetric0 pER_Transitive0 -> Build_PER (\x y x0 ->
    pER_Symmetric0 y x x0) (\x y z x0 x1 -> pER_Transitive0 z y x x1 x0)}

flip_Equivalence :: (Equivalence a1 a2) -> Equivalence a1 a2
flip_Equivalence h =
  case h of {
   Build_Equivalence equivalence_Reflexive0 equivalence_Symmetric0
    equivalence_Transitive0 -> Build_Equivalence equivalence_Reflexive0 (\x y x0 ->
    equivalence_Symmetric0 y x x0) (\x y z x0 x1 ->
    equivalence_Transitive0 z y x x1 x0)}

eq_equivalence :: Equivalence a1 ()
eq_equivalence =
  Build_Equivalence __ __ __

iff_equivalence :: Equivalence () ()
iff_equivalence =
  Build_Equivalence __ __ __

arrow_Reflexive_obligation_1 :: Coq_arrow a1 a1
arrow_Reflexive_obligation_1 x =
  x

arrow_Reflexive :: Coq_arrow a1 a1
arrow_Reflexive =
  arrow_Reflexive_obligation_1

arrow_Transitive_obligation_1 :: (Coq_arrow a1 a2) -> (Coq_arrow a2 a3) -> Coq_arrow
                                 a1 a3
arrow_Transitive_obligation_1 x x0 x1 =
  x0 (x x1)

arrow_Transitive :: (Coq_arrow a1 a2) -> (Coq_arrow a2 a3) -> Coq_arrow a1 a3
arrow_Transitive =
  arrow_Transitive_obligation_1

iffT_Reflexive :: Coq_iffT a1 a1
iffT_Reflexive =
  (,) (\x -> x) (\x -> x)

iffT_Symmetric :: (Coq_iffT a1 a2) -> Coq_iffT a2 a1
iffT_Symmetric x =
  Datatypes.prod_rect (\a b -> (,) b a) x

iffT_Transitive :: (Coq_iffT a1 a2) -> (Coq_iffT a2 a3) -> Coq_iffT a1 a3
iffT_Transitive x x0 =
  Datatypes.prod_rect (\a b ->
    Datatypes.prod_rect (\a0 b0 -> (,) (\x1 -> let {x2 = a0 x1} in a x2) (\x1 ->
      let {x2 = b x1} in b0 x2)) x) x0

type Coq_relation_equivalence a x0 x = a -> a -> Coq_iffT x0 x

type Coq_relation_conjunction a r x = (,) r x

type Coq_relation_disjunction a r x = Prelude.Either r x

relation_equivalence_equivalence :: Equivalence (Coq_crelation a1)
                                    (Coq_relation_equivalence a1 Any Any)
relation_equivalence_equivalence =
  Build_Equivalence (\_ _ _ -> (,) (\x -> x) (\x -> x)) (\_ _ x x0 y0 -> (,) (\x1 ->
    let {x2 = x x0} in let {x3 = x2 y0} in Datatypes.prod_rect (\_ b -> b x1) x3)
    (\x1 ->
    let {x2 = x x0} in let {x3 = x2 y0} in Datatypes.prod_rect (\a _ -> a x1) x3))
    (\_ _ _ x x0 x1 y0 ->
    let {x2 = x x1 y0} in
    let {x3 = x0 x1 y0} in
    Datatypes.prod_rect (\a b ->
      Datatypes.prod_rect (\a0 b0 -> (,) (\x4 -> let {x5 = a0 x4} in a x5) (\x4 ->
        let {x5 = b x4} in b0 x5)) x2) x3)

relation_implication_preorder :: PreOrder (Coq_crelation a1)
                                 (Coq_subrelation a1 Any Any)
relation_implication_preorder =
  Build_PreOrder (\_ _ _ x -> x) (\_ _ _ x x0 x1 y0 x2 -> x0 x1 y0 (x x1 y0 x2))

type PartialOrder a eqA r =
  Coq_relation_equivalence a eqA (Coq_relation_conjunction a r r)

partial_order_equivalence :: (Equivalence a1 a2) -> (PreOrder a1 a3) ->
                             (PartialOrder a1 a2 a3) -> Coq_relation_equivalence 
                             a1 a2 (Coq_relation_conjunction a1 a3 a3)
partial_order_equivalence _ _ partialOrder =
  partialOrder

partial_order_antisym :: (Equivalence a1 a2) -> (PreOrder a1 a3) -> (PartialOrder 
                         a1 a2 a3) -> Antisymmetric a1 a2 a3
partial_order_antisym _ _ h x y x0 x1 =
  case h x y of {
   (,) _ x2 -> x2 ((,) x0 x1)}

coq_PartialOrder_inverse :: (Equivalence a1 a2) -> (PreOrder a1 a3) -> (PartialOrder
                            a1 a2 a3) -> PartialOrder a1 a2 a3
coq_PartialOrder_inverse equ _ h x y =
  (,) (\x0 ->
    case h y x of {
     (,) x1 _ -> x1 (symmetry (coq_Equivalence_Symmetric equ) x y x0)}) (\x0 ->
    case x0 of {
     (,) h1 h2 -> case h x y of {
                   (,) _ x1 -> x1 ((,) h2 h1)}})

