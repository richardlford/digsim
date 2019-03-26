module String0 where

import qualified Prelude

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

substring :: Prelude.Integer -> Prelude.Integer -> Prelude.String -> Prelude.String
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

index :: Prelude.Integer -> Prelude.String -> Prelude.String -> Prelude.Maybe Prelude.Integer
index n s1 s2 =
  case s2 of {
   ([]) ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> case s1 of {
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

