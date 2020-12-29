module Wf where

import qualified Prelude

__ :: any
__ = Prelude.error "Logical or arity value used"

coq_Acc_rect :: (a1 -> () -> (a1 -> () -> a2) -> a2) -> a1 -> a2
coq_Acc_rect f x =
  f x __ (\y _ -> coq_Acc_rect f y)

coq_Acc_rec :: (a1 -> () -> (a1 -> () -> a2) -> a2) -> a1 -> a2
coq_Acc_rec =
  coq_Acc_rect

well_founded_induction_type :: (a1 -> (a1 -> () -> a2) -> a2) -> a1 -> a2
well_founded_induction_type x a =
  coq_Acc_rect (\x0 _ x1 -> x x0 x1) a

well_founded_induction :: (a1 -> (a1 -> () -> a2) -> a2) -> a1 -> a2
well_founded_induction =
  well_founded_induction_type

coq_Fix_F :: (a1 -> (a1 -> () -> a2) -> a2) -> a1 -> a2
coq_Fix_F f x =
  f x (\y _ -> coq_Fix_F f y)

coq_Fix :: (a1 -> (a1 -> () -> a2) -> a2) -> a1 -> a2
coq_Fix =
  coq_Fix_F

coq_Fix_F_2 :: (a1 -> a2 -> (a1 -> a2 -> () -> a3) -> a3) -> a1 -> a2 -> a3
coq_Fix_F_2 f x x' =
  f x x' (\y y' _ -> coq_Fix_F_2 f y y')

well_founded_induction_type_2 :: (a1 -> a2 -> (a1 -> a2 -> () -> a3) -> a3) -> a1 ->
                                 a2 -> a3
well_founded_induction_type_2 =
  coq_Fix_F_2

