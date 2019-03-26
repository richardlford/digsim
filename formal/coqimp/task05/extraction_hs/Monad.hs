{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module Monad where

import qualified Prelude

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

#ifdef __GLASGOW_HASKELL__
type Any = GHC.Base.Any
#else
-- HUGS
type Any = ()
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

data MonadOps m =
   Build_MonadOps (() -> () -> m -> (Any -> m) -> m) (() -> Any -> m)

bind :: (MonadOps a1) -> a1 -> (a2 -> a1) -> a1
bind monadOps x x0 =
  case monadOps of {
   Build_MonadOps bind0 _ -> unsafeCoerce bind0 __ __ x x0}

ret :: (MonadOps a1) -> a2 -> a1
ret monadOps x =
  case monadOps of {
   Build_MonadOps _ ret0 -> unsafeCoerce ret0 __ x}

data MonadTransformerOps t =
   Build_MonadTransformerOps (() -> (MonadOps Any) -> MonadOps t) (() -> (MonadOps Any) -> () -> Any
                                                                  -> t)

coq_Tops :: (MonadTransformerOps a1) -> (MonadOps a2) -> MonadOps a1
coq_Tops monadTransformerOps h =
  case monadTransformerOps of {
   Build_MonadTransformerOps tops _ -> unsafeCoerce tops __ h}

idM :: MonadOps Any
idM =
  Build_MonadOps (\_ _ x f -> f x) (\_ x -> x)

monad_of_transformer :: (MonadTransformerOps a1) -> MonadOps a1
monad_of_transformer h =
  coq_Tops h idM

optionT :: MonadTransformerOps Any
optionT =
  Build_MonadTransformerOps (\_ h -> Build_MonadOps (\_ _ x f ->
    bind h x (\a -> case a of {
                     Prelude.Just a' -> f a';
                     Prelude.Nothing -> ret h Prelude.Nothing})) (\_ x -> ret h (Prelude.Just x)))
    (\_ h _ x -> bind h x (\a -> ret h (Prelude.Just a)))

optionM :: MonadOps (Prelude.Maybe Any)
optionM =
  monad_of_transformer (unsafeCoerce optionT)

