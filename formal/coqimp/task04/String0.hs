module String0 where

import qualified Prelude
import qualified Ascii
import qualified Bool
import qualified Logic

string_rect :: a1 -> (Prelude.Char -> Prelude.String -> a1 -> a1) ->
               Prelude.String -> a1
string_rect f f0 s =
  case s of {
   ([]) -> f;
   (:) a s0 -> f0 a s0 (string_rect f f0 s0)}

string_rec :: a1 -> (Prelude.Char -> Prelude.String -> a1 -> a1) ->
              Prelude.String -> a1
string_rec =
  string_rect

eqb_spec :: Prelude.String -> Prelude.String -> Bool.Coq_reflect
eqb_spec s1 s2 =
  string_rec (\s3 ->
    case s3 of {
     ([]) -> Bool.ReflectT;
     (:) _ _ -> Bool.ReflectF}) (\a s3 iHs1 s4 ->
    case s4 of {
     ([]) -> Bool.ReflectF;
     (:) a0 s5 ->
      case Ascii.eqb_spec a a0 of {
       Bool.ReflectT ->
        Logic.eq_rec_r a0
          (case iHs1 s5 of {
            Bool.ReflectT -> Logic.eq_rec_r s5 (\_ -> Bool.ReflectT) s3 iHs1;
            Bool.ReflectF -> Bool.ReflectF}) a;
       Bool.ReflectF -> Bool.ReflectF}}) s1 s2

append :: Prelude.String -> Prelude.String -> Prelude.String
append s1 s2 =
  case s1 of {
   ([]) -> s2;
   (:) c s1' -> (:) c (append s1' s2)}

length :: Prelude.String -> Prelude.Integer
length s =
  case s of {
   ([]) -> 0;
   (:) _ s' -> Prelude.succ (length s')}

get :: Prelude.Integer -> Prelude.String -> Prelude.Maybe Prelude.Char
get n s =
  case s of {
   ([]) -> Prelude.Nothing;
   (:) c s' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.Just c)
      (\n' -> get n' s')
      n}

substring :: Prelude.Integer -> Prelude.Integer -> Prelude.String ->
             Prelude.String
substring n m s =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> ([]))
      (\m' -> case s of {
               ([]) -> s;
               (:) c s' -> (:) c (substring 0 m' s')})
      m)
    (\n' -> case s of {
             ([]) -> s;
             (:) _ s' -> substring n' m s'})
    n

concat :: Prelude.String -> (([]) Prelude.String) -> Prelude.String
concat sep ls =
  case ls of {
   ([]) -> ([]);
   (:) x xs ->
    case xs of {
     ([]) -> x;
     (:) _ _ -> append x (append sep (concat sep xs))}}

prefix :: Prelude.String -> Prelude.String -> Prelude.Bool
prefix s1 s2 =
  case s1 of {
   ([]) -> Prelude.True;
   (:) a s1' ->
    case s2 of {
     ([]) -> Prelude.False;
     (:) b s2' ->
      case (Prelude.==) a b of {
       Prelude.True -> prefix s1' s2';
       Prelude.False -> Prelude.False}}}

index :: Prelude.Integer -> Prelude.String -> Prelude.String -> Prelude.Maybe
         Prelude.Integer
index n s1 s2 =
  case s2 of {
   ([]) ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      case s1 of {
       ([]) -> Prelude.Just 0;
       (:) _ _ -> Prelude.Nothing})
      (\_ -> Prelude.Nothing)
      n;
   (:) _ s2' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      case prefix s1 s2 of {
       Prelude.True -> Prelude.Just 0;
       Prelude.False ->
        case index 0 s1 s2' of {
         Prelude.Just n0 -> Prelude.Just (Prelude.succ n0);
         Prelude.Nothing -> Prelude.Nothing}})
      (\n' ->
      case index n' s1 s2' of {
       Prelude.Just n0 -> Prelude.Just (Prelude.succ n0);
       Prelude.Nothing -> Prelude.Nothing})
      n}

findex :: Prelude.Integer -> Prelude.String -> Prelude.String ->
          Prelude.Integer
findex n s1 s2 =
  case index n s1 s2 of {
   Prelude.Just n0 -> n0;
   Prelude.Nothing -> 0}

coq_HelloWorld :: Prelude.String
coq_HelloWorld =
  (:) '\t' ((:) '"' ((:) 'H' ((:) 'e' ((:) 'l' ((:) 'l' ((:) 'o' ((:) ' '
    ((:) 'w' ((:) 'o' ((:) 'r' ((:) 'l' ((:) 'd' ((:) '!' ((:) '"' ((:)
    '\007' ((:) '\n' ([])))))))))))))))))

