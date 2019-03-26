module Model_data where

import qualified Prelude

data Coq_stateVar =
   SvT
 | SvX
 | SvXD
 | SvXDD
 | SvCOEFF_OF_REST
 | SvGRAVITY
 | SvT_STOP
 | SvDT
 | SvDT_MAX
 | SvDT_MIN
 | SvDT_PRINT

stateVar_rect :: a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> Coq_stateVar -> a1
stateVar_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 s =
  case s of {
   SvT -> f;
   SvX -> f0;
   SvXD -> f1;
   SvXDD -> f2;
   SvCOEFF_OF_REST -> f3;
   SvGRAVITY -> f4;
   SvT_STOP -> f5;
   SvDT -> f6;
   SvDT_MAX -> f7;
   SvDT_MIN -> f8;
   SvDT_PRINT -> f9}

stateVar_rec :: a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> Coq_stateVar -> a1
stateVar_rec =
  stateVar_rect

state_var_eq :: Coq_stateVar -> Coq_stateVar -> Prelude.Bool
state_var_eq r1 r2 =
  stateVar_rec (\x -> case x of {
                       SvT -> Prelude.True;
                       _ -> Prelude.False}) (\x ->
    case x of {
     SvX -> Prelude.True;
     _ -> Prelude.False}) (\x -> case x of {
                                  SvXD -> Prelude.True;
                                  _ -> Prelude.False}) (\x ->
    case x of {
     SvXDD -> Prelude.True;
     _ -> Prelude.False}) (\x -> case x of {
                                  SvCOEFF_OF_REST -> Prelude.True;
                                  _ -> Prelude.False}) (\x ->
    case x of {
     SvGRAVITY -> Prelude.True;
     _ -> Prelude.False}) (\x -> case x of {
                                  SvT_STOP -> Prelude.True;
                                  _ -> Prelude.False}) (\x ->
    case x of {
     SvDT -> Prelude.True;
     _ -> Prelude.False}) (\x -> case x of {
                                  SvDT_MAX -> Prelude.True;
                                  _ -> Prelude.False}) (\x ->
    case x of {
     SvDT_MIN -> Prelude.True;
     _ -> Prelude.False}) (\x -> case x of {
                                  SvDT_PRINT -> Prelude.True;
                                  _ -> Prelude.False}) r1 r2

svIndex :: Coq_stateVar -> Prelude.Integer
svIndex sv =
  case sv of {
   SvT -> 1;
   SvX -> (\x -> 2 Prelude.* x) 1;
   SvXD -> (\x -> 2 Prelude.* x Prelude.+ 1) 1;
   SvXDD -> (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1);
   SvCOEFF_OF_REST -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1);
   SvGRAVITY -> (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1);
   SvT_STOP -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1) 1);
   SvDT -> (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1));
   SvDT_MAX -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1));
   SvDT_MIN -> (\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1));
   SvDT_PRINT -> (\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1))}

svStrList :: ([]) ((,) Coq_stateVar Prelude.String)
svStrList =
  (:) ((,) SvT ((:) 'T' ([]))) ((:) ((,) SvX ((:) 'X' ([]))) ((:) ((,) SvXD ((:) 'X' ((:) 'D' ([]))))
    ((:) ((,) SvXDD ((:) 'X' ((:) 'D' ((:) 'D' ([]))))) ((:) ((,) SvCOEFF_OF_REST ((:) 'C' ((:) 'O'
    ((:) 'E' ((:) 'F' ((:) 'F' ((:) '_' ((:) 'O' ((:) 'F' ((:) '_' ((:) 'R' ((:) 'E' ((:) 'S' ((:)
    'T' ([]))))))))))))))) ((:) ((,) SvGRAVITY ((:) 'G' ((:) 'R' ((:) 'A' ((:) 'V' ((:) 'I' ((:) 'T'
    ((:) 'Y' ([]))))))))) ((:) ((,) SvT_STOP ((:) 'T' ((:) '_' ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P'
    ([])))))))) ((:) ((,) SvDT ((:) 'D' ((:) 'T' ([])))) ((:) ((,) SvDT_MAX ((:) 'D' ((:) 'T' ((:)
    '_' ((:) 'M' ((:) 'A' ((:) 'X' ([])))))))) ((:) ((,) SvDT_MIN ((:) 'D' ((:) 'T' ((:) '_' ((:) 'M'
    ((:) 'I' ((:) 'N' ([])))))))) ((:) ((,) SvDT_PRINT ((:) 'D' ((:) 'T' ((:) '_' ((:) 'P' ((:) 'R'
    ((:) 'I' ((:) 'N' ((:) 'T' ([])))))))))) ([])))))))))))

driver_defaults_str :: ([]) ((,) Coq_stateVar Prelude.String)
driver_defaults_str =
  (:) ((,) SvT ((:) '0' ((:) '.' ((:) '0' ([]))))) ((:) ((,) SvT_STOP ((:) '0' ((:) '.' ((:) '0'
    ([]))))) ((:) ((,) SvDT ((:) '0' ((:) '.' ((:) '0' ((:) '0' ((:) '5' ([]))))))) ((:) ((,)
    SvDT_MAX ((:) '0' ((:) '.' ((:) '0' ((:) '0' ((:) '5' ([]))))))) ((:) ((,) SvDT_MIN ((:) '0' ((:)
    '.' ((:) '0' ((:) '0' ((:) '5' ([]))))))) ((:) ((,) SvDT_PRINT ((:) '0' ((:) '.' ((:) '0' ((:)
    '1' ([])))))) ([]))))))

model_default_values_str :: ([]) ((,) Coq_stateVar Prelude.String)
model_default_values_str =
  (:) ((,) SvT ((:) '0' ((:) '.' ((:) '0' ([]))))) ((:) ((,) SvX ((:) '1' ((:) '0' ((:) '.' ((:) '0'
    ([])))))) ((:) ((,) SvXD ((:) '0' ((:) '.' ((:) '0' ([]))))) ((:) ((,) SvXDD ((:) '0' ((:) '.'
    ((:) '0' ([]))))) ((:) ((,) SvCOEFF_OF_REST ((:) '0' ((:) '.' ((:) '8' ((:) '0' ([])))))) ((:)
    ((,) SvGRAVITY ((:) '9' ((:) '.' ((:) '8' ((:) '8' ([])))))) ((:) ((,) SvT_STOP ((:) '1' ((:) '0'
    ((:) '.' ((:) '0' ([])))))) ((:) ((,) SvDT ((:) '0' ((:) '.' ((:) '0' ((:) '1' ([])))))) ((:)
    ((,) SvDT_MAX ((:) '0' ((:) '.' ((:) '0' ((:) '0' ((:) '5' ([]))))))) ((:) ((,) SvDT_MIN ((:) '0'
    ((:) '.' ((:) '0' ((:) '0' ((:) '1' ([]))))))) ((:) ((,) SvDT_PRINT ((:) '0' ((:) '.' ((:) '0'
    ((:) '1' ([])))))) ([])))))))))))

modelOutputs :: ([]) Coq_stateVar
modelOutputs =
  (:) SvT ((:) SvX ((:) SvXD ([])))

