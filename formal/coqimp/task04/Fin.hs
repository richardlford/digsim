module Fin where

import qualified Prelude
import qualified Datatypes
import qualified Logic

__ :: any
__ = Prelude.error "Logical or arity value used"

data Coq_t =
   F1 Prelude.Integer
 | FS Prelude.Integer Coq_t

t_rect :: (Prelude.Integer -> a1) -> (Prelude.Integer -> Coq_t -> a1 -> a1)
          -> Prelude.Integer -> Coq_t -> a1
t_rect f f0 _ t =
  case t of {
   F1 n -> f n;
   FS n t0 -> f0 n t0 (t_rect f f0 n t0)}

t_rec :: (Prelude.Integer -> a1) -> (Prelude.Integer -> Coq_t -> a1 -> a1) ->
         Prelude.Integer -> Coq_t -> a1
t_rec =
  t_rect

case0 :: Coq_t -> a1
case0 _ =
  __

caseS' :: Prelude.Integer -> Coq_t -> a1 -> (Coq_t -> a1) -> a1
caseS' _ p p1 pS =
  case p of {
   F1 _ -> p1;
   FS _ pp -> pS pp}

caseS :: (Prelude.Integer -> a1) -> (Prelude.Integer -> Coq_t -> a1) ->
         Prelude.Integer -> Coq_t -> a1
caseS p1 pS n p =
  caseS' n p (p1 n) (pS n)

rectS :: (Prelude.Integer -> a1) -> (Prelude.Integer -> Coq_t -> a1 -> a1) ->
         Prelude.Integer -> Coq_t -> a1
rectS p1 pS _ p =
  case p of {
   F1 k -> p1 k;
   FS n pp ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> case0 pp)
      (\k -> pS k pp (rectS p1 pS k pp))
      n}

rect2 :: (Prelude.Integer -> a1) -> (Prelude.Integer -> Coq_t -> a1) ->
         (Prelude.Integer -> Coq_t -> a1) -> (Prelude.Integer -> Coq_t ->
         Coq_t -> a1 -> a1) -> Prelude.Integer -> Coq_t -> Coq_t -> a1
rect2 h0 h1 h2 hS _ a b =
  case a of {
   F1 m -> caseS' m b (h0 m) (h1 m);
   FS m a' ->
    caseS' m b (h2 m a') (\b' -> hS m a' b' (rect2 h0 h1 h2 hS m a' b'))}

to_nat :: Prelude.Integer -> Coq_t -> Prelude.Integer
to_nat _ n =
  case n of {
   F1 _ -> 0;
   FS n0 p -> Prelude.succ (to_nat n0 p)}

of_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Maybe Coq_t
of_nat p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.Nothing)
    (\n' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.Just (F1 n'))
      (\p' ->
      case of_nat p' n' of {
       Prelude.Just f -> Prelude.Just (FS n' f);
       Prelude.Nothing -> Prelude.Nothing})
      p)
    n

of_nat_lt :: Prelude.Integer -> Prelude.Integer -> Coq_t
of_nat_lt p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Logic.coq_False_rect)
    (\n' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> F1 n')
      (\p' -> FS n' (of_nat_lt p' n'))
      p)
    n

weak :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> (Coq_t ->
        Coq_t) -> Coq_t -> Coq_t
weak m n p f =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> f)
    (\p' x ->
    case x of {
     F1 _ -> F1 ((Prelude.+) p' n);
     FS n' y -> FS ((Prelude.+) p' n)
      (weak m n p' f (Logic.eq_rect n' y ((Prelude.+) p' m)))})
    p

coq_L :: Prelude.Integer -> Prelude.Integer -> Coq_t -> Coq_t
coq_L _ n p =
  case p of {
   F1 n0 -> F1
    (let {
      add n1 m =
        (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
          (\_ -> m)
          (\p0 -> Prelude.succ (add p0 m))
          n1}
     in add n0 n);
   FS n0 p' -> FS ((Prelude.+) n0 n) (coq_L n0 n p')}

coq_L_R :: Prelude.Integer -> Prelude.Integer -> Coq_t -> Coq_t
coq_L_R m n p =
  Datatypes.nat_rec p (\n0 iHn ->
    let {
     lS _ p0 =
       case p0 of {
        F1 k' -> F1 (Prelude.succ k');
        FS n1 p' -> FS (Prelude.succ n1) (lS n1 p')}}
    in lS ((Prelude.+) n0 m) iHn) n

coq_R :: Prelude.Integer -> Prelude.Integer -> Coq_t -> Coq_t
coq_R m n p =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> p)
    (\n' -> FS ((Prelude.+) n' m) (coq_R m n' p))
    n

depair :: Prelude.Integer -> Prelude.Integer -> Coq_t -> Coq_t -> Coq_t
depair _ n o p =
  case o of {
   F1 m' -> coq_L n ((Prelude.*) m' n) p;
   FS n0 o' -> coq_R ((Prelude.*) n0 n) n (depair n0 n o' p)}

eqb :: Prelude.Integer -> Prelude.Integer -> Coq_t -> Coq_t -> Prelude.Bool
eqb _ _ p q =
  case p of {
   F1 m' -> case q of {
             F1 n' -> (Prelude.==) m' n';
             FS _ _ -> Prelude.False};
   FS n p' -> case q of {
               F1 _ -> Prelude.False;
               FS n0 q' -> eqb n n0 p' q'}}

eq_dec :: Prelude.Integer -> Coq_t -> Coq_t -> Prelude.Bool
eq_dec n x y =
  case eqb n n x y of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

cast :: Prelude.Integer -> Coq_t -> Prelude.Integer -> Coq_t
cast _ v n =
  case v of {
   F1 _ ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Logic.coq_False_rect)
      (\n' -> F1 n')
      n;
   FS n0 f ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Logic.coq_False_rect)
      (\n' -> FS n' (cast n0 f n'))
      n}

