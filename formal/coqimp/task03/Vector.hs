module Vector where

import qualified Prelude
import qualified Datatypes
import qualified Fin
import qualified Logic
import qualified Nat
import qualified Plus
import qualified VectorDef

__ :: any
__ = Prelude.error "Logical or arity value used"

data Coq_t a =
   Coq_nil
 | Coq_cons a Prelude.Integer (Coq_t a)

t_rect :: a2 -> (a1 -> Prelude.Integer -> (Coq_t a1) -> a2 -> a2) -> Prelude.Integer
          -> (Coq_t a1) -> a2
t_rect f f0 _ t =
  case t of {
   Coq_nil -> f;
   Coq_cons h n t0 -> f0 h n t0 (t_rect f f0 n t0)}

t_rec :: a2 -> (a1 -> Prelude.Integer -> (Coq_t a1) -> a2 -> a2) -> Prelude.Integer
         -> (Coq_t a1) -> a2
t_rec =
  t_rect

rectS :: (a1 -> a2) -> (a1 -> Prelude.Integer -> (Coq_t a1) -> a2 -> a2) ->
         Prelude.Integer -> (Coq_t a1) -> a2
rectS bas rect _ v =
  case v of {
   Coq_nil -> __;
   Coq_cons a n v0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> case v0 of {
              Coq_nil -> bas a;
              Coq_cons _ _ _ -> __})
      (\nn' -> rect a nn' v0 (rectS bas rect nn' v0))
      n}

case0 :: a2 -> (Coq_t a1) -> a2
case0 h v =
  case v of {
   Coq_nil -> h;
   Coq_cons _ _ _ -> __}

caseS :: (a1 -> Prelude.Integer -> (Coq_t a1) -> a2) -> Prelude.Integer -> (Coq_t
         a1) -> a2
caseS h _ v =
  case v of {
   Coq_nil -> __;
   Coq_cons h0 n t -> h h0 n t}

caseS' :: Prelude.Integer -> (Coq_t a1) -> (a1 -> (Coq_t a1) -> a2) -> a2
caseS' _ v h =
  case v of {
   Coq_nil -> __;
   Coq_cons h0 _ t -> h h0 t}

rect2 :: a3 -> (Prelude.Integer -> (Coq_t a1) -> (Coq_t a2) -> a3 -> a1 -> a2 -> a3)
         -> Prelude.Integer -> (Coq_t a1) -> (Coq_t a2) -> a3
rect2 bas rect _ v1 v2 =
  case v1 of {
   Coq_nil -> case0 bas v2;
   Coq_cons h1 n' t1 ->
    caseS' n' v2 (\h2 t2 -> rect n' t1 t2 (rect2 bas rect n' t1 t2) h1 h2)}

hd :: Prelude.Integer -> (Coq_t a1) -> a1
hd =
  caseS (\h _ _ -> h)

last :: Prelude.Integer -> (Coq_t a1) -> a1
last =
  rectS (\a -> a) (\_ _ _ h -> h)

const :: a1 -> Prelude.Integer -> Coq_t a1
const a =
  Datatypes.nat_rect Coq_nil (\n x -> Coq_cons a n x)

nth :: Prelude.Integer -> (Coq_t a1) -> Fin.Coq_t -> a1
nth _ v' p =
  case p of {
   Fin.F1 n -> caseS (\h _ _ -> h) n v';
   Fin.FS n p' -> caseS (\_ -> nth) n v' p'}

nth_order :: Prelude.Integer -> (Coq_t a1) -> Prelude.Integer -> a1
nth_order n v p =
  nth n v (Fin.of_nat_lt p n)

replace :: Prelude.Integer -> (Coq_t a1) -> Fin.Coq_t -> a1 -> Coq_t a1
replace _ v p a =
  case p of {
   Fin.F1 k -> caseS' k v (\_ t -> Coq_cons a k t);
   Fin.FS k p' -> caseS' k v (\h t -> Coq_cons h k (replace k t p' a))}

replace_order :: Prelude.Integer -> (Coq_t a1) -> Prelude.Integer -> a1 -> Coq_t a1
replace_order n v p =
  replace n v (Fin.of_nat_lt p n)

tl :: Prelude.Integer -> (Coq_t a1) -> Coq_t a1
tl =
  caseS (\_ _ t -> t)

shiftout :: Prelude.Integer -> (Coq_t a1) -> Coq_t a1
shiftout =
  rectS (\_ -> Coq_nil) (\h n _ h0 -> Coq_cons h n h0)

shiftin :: Prelude.Integer -> a1 -> (Coq_t a1) -> Coq_t a1
shiftin _ a v =
  case v of {
   Coq_nil -> Coq_cons a 0 Coq_nil;
   Coq_cons h n t -> Coq_cons h (Prelude.succ n) (shiftin n a t)}

shiftrepeat :: Prelude.Integer -> (Coq_t a1) -> Coq_t a1
shiftrepeat =
  rectS (\h -> Coq_cons h (Prelude.succ 0) (Coq_cons h 0 Coq_nil)) (\h n _ h0 ->
    Coq_cons h (Prelude.succ (Prelude.succ n)) h0)

take :: Prelude.Integer -> Prelude.Integer -> (Coq_t a1) -> Coq_t a1
take _ p v =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Coq_nil)
    (\p' ->
    case v of {
     Coq_nil -> Logic.coq_False_rect;
     Coq_cons x n xs -> Coq_cons x p' (take n p' xs)})
    p

trunc :: Prelude.Integer -> Prelude.Integer -> (Coq_t a1) -> Coq_t a1
trunc n p x =
  Datatypes.nat_rect (\_ v -> Logic.eq_rect n v (Nat.sub n 0)) (\p0 f _ v ->
    shiftout (Nat.sub n (Prelude.succ p0))
      (Logic.eq_rect_r (Nat.sub (Prelude.succ n) (Prelude.succ p0)) (f __ v)
        (Prelude.succ (Nat.sub n (Prelude.succ p0))))) p __ x

append :: Prelude.Integer -> Prelude.Integer -> (Coq_t a1) -> (Coq_t a1) -> Coq_t a1
append _ p v w =
  case v of {
   Coq_nil -> w;
   Coq_cons a n v' -> Coq_cons a ((Prelude.+) n p) (append n p v' w)}

rev_append_tail :: Prelude.Integer -> Prelude.Integer -> (Coq_t a1) -> (Coq_t 
                   a1) -> Coq_t a1
rev_append_tail _ p v w =
  case v of {
   Coq_nil -> w;
   Coq_cons a n v' -> rev_append_tail n (Prelude.succ p) v' (Coq_cons a p w)}

rev_append :: Prelude.Integer -> Prelude.Integer -> (Coq_t a1) -> (Coq_t a1) ->
              Coq_t a1
rev_append n p v w =
  Logic.eq_rect_r (Plus.tail_plus n p) (rev_append_tail n p v w) ((Prelude.+) n p)

rev :: Prelude.Integer -> (Coq_t a1) -> Coq_t a1
rev n v =
  Logic.eq_rect_r ((Prelude.+) n 0) (rev_append n 0 v Coq_nil) n

map :: (a1 -> a2) -> Prelude.Integer -> (Coq_t a1) -> Coq_t a2
map f _ v =
  case v of {
   Coq_nil -> Coq_nil;
   Coq_cons a n v' -> Coq_cons (f a) n (map f n v')}

map2 :: (a1 -> a2 -> a3) -> Prelude.Integer -> (Coq_t a1) -> (Coq_t a2) -> Coq_t a3
map2 g =
  rect2 Coq_nil (\n _ _ h a b -> Coq_cons (g a b) n h)

fold_left :: (a2 -> a1 -> a2) -> a2 -> Prelude.Integer -> (Coq_t a1) -> a2
fold_left f b _ v =
  case v of {
   Coq_nil -> b;
   Coq_cons a n w -> fold_left f (f b a) n w}

fold_right :: (a1 -> a2 -> a2) -> Prelude.Integer -> (Coq_t a1) -> a2 -> a2
fold_right f _ v b =
  case v of {
   Coq_nil -> b;
   Coq_cons a n w -> f a (fold_right f n w b)}

fold_right2 :: (a1 -> a2 -> a3 -> a3) -> a3 -> Prelude.Integer -> (Coq_t a1) ->
               (Coq_t a2) -> a3
fold_right2 g c =
  rect2 c (\_ _ _ h a b -> g a b h)

fold_left2 :: (a1 -> a2 -> a3 -> a1) -> a1 -> Prelude.Integer -> (Coq_t a2) ->
              (Coq_t a3) -> a1
fold_left2 f a _ v w =
  case v of {
   Coq_nil -> case0 a w;
   Coq_cons vh vn vt -> caseS' vn w (\wh wt -> fold_left2 f (f a vh wh) vn vt wt)}

of_list :: (([]) a1) -> Coq_t a1
of_list l =
  case l of {
   ([]) -> Coq_nil;
   (:) h tail -> Coq_cons h (Datatypes.length tail) (of_list tail)}

to_list :: Prelude.Integer -> (Coq_t a1) -> ([]) a1
to_list n v =
  let {
   fold_right_fix _ v0 b =
     case v0 of {
      Coq_nil -> b;
      Coq_cons a n0 w -> (:) a (fold_right_fix n0 w b)}}
  in fold_right_fix n v ([])

eqb :: (a1 -> a1 -> Prelude.Bool) -> Prelude.Integer -> Prelude.Integer ->
       (VectorDef.Coq_t a1) -> (VectorDef.Coq_t a1) -> Prelude.Bool
eqb a_beq _ _ v1 v2 =
  case v1 of {
   VectorDef.Coq_nil ->
    case v2 of {
     VectorDef.Coq_nil -> Prelude.True;
     VectorDef.Coq_cons _ _ _ -> Prelude.False};
   VectorDef.Coq_cons h1 n t1 ->
    case v2 of {
     VectorDef.Coq_nil -> Prelude.False;
     VectorDef.Coq_cons h2 n0 t2 ->
      (Prelude.&&) (a_beq h1 h2) (eqb a_beq n n0 t1 t2)}}

eq_dec :: (a1 -> a1 -> Prelude.Bool) -> Prelude.Integer -> (VectorDef.Coq_t 
          a1) -> (VectorDef.Coq_t a1) -> Prelude.Bool
eq_dec a_beq n v1 v2 =
  case eqb a_beq n n v1 v2 of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

cast :: Prelude.Integer -> (VectorDef.Coq_t a1) -> Prelude.Integer ->
        VectorDef.Coq_t a1
cast _ v n =
  case v of {
   VectorDef.Coq_nil ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> VectorDef.Coq_nil)
      (\_ -> Logic.coq_False_rect)
      n;
   VectorDef.Coq_cons h n0 w ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Logic.coq_False_rect)
      (\n' -> VectorDef.Coq_cons h n' (cast n0 w n'))
      n}

