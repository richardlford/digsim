module Fappli_IEEE where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified Bool
import qualified Datatypes
import qualified Fcalc_bracket
import qualified Fcalc_round
import qualified Fcore_FLT
import qualified Fcore_Zaux
import qualified Fcore_digits
import qualified Zbool
import qualified Zpower

data Coq_full_float =
   F754_zero Prelude.Bool
 | F754_infinity Prelude.Bool
 | F754_nan Prelude.Bool Prelude.Integer
 | F754_finite Prelude.Bool Prelude.Integer Prelude.Integer

type Coq_nan_pl = Prelude.Integer

data Coq_binary_float =
   B754_zero Prelude.Bool
 | B754_infinity Prelude.Bool
 | B754_nan Prelude.Bool Coq_nan_pl
 | B754_finite Prelude.Bool Prelude.Integer Prelude.Integer

coq_FF2B :: Prelude.Integer -> Prelude.Integer -> Coq_full_float -> Coq_binary_float
coq_FF2B _ _ x =
  case x of {
   F754_zero s -> B754_zero s;
   F754_infinity s -> B754_infinity s;
   F754_nan b pl -> B754_nan b pl;
   F754_finite s m e -> B754_finite s m e}

coq_Bopp :: Prelude.Integer -> Prelude.Integer -> (Prelude.Bool -> Coq_nan_pl -> (,) Prelude.Bool
            Coq_nan_pl) -> Coq_binary_float -> Coq_binary_float
coq_Bopp _ _ opp_nan x =
  case x of {
   B754_zero sx -> B754_zero (Prelude.not sx);
   B754_infinity sx -> B754_infinity (Prelude.not sx);
   B754_nan sx plx -> case opp_nan sx plx of {
                       (,) sres plres -> B754_nan sres plres};
   B754_finite sx mx ex -> B754_finite (Prelude.not sx) mx ex}

coq_Babs :: Prelude.Integer -> Prelude.Integer -> (Prelude.Bool -> Coq_nan_pl -> (,) Prelude.Bool
            Coq_nan_pl) -> Coq_binary_float -> Coq_binary_float
coq_Babs _ _ abs_nan x =
  case x of {
   B754_zero _ -> B754_zero Prelude.False;
   B754_infinity _ -> B754_infinity Prelude.False;
   B754_nan sx plx -> case abs_nan sx plx of {
                       (,) sres plres -> B754_nan sres plres};
   B754_finite _ mx ex -> B754_finite Prelude.False mx ex}

coq_Bcompare :: Prelude.Integer -> Prelude.Integer -> Coq_binary_float -> Coq_binary_float ->
                Prelude.Maybe Datatypes.Coq_comparison
coq_Bcompare _ _ f1 f2 =
  case f1 of {
   B754_zero _ ->
    case f2 of {
     B754_zero _ -> Prelude.Just Datatypes.Eq;
     B754_infinity b ->
      case b of {
       Prelude.True -> Prelude.Just Datatypes.Gt;
       Prelude.False -> Prelude.Just Datatypes.Lt};
     B754_nan _ _ -> Prelude.Nothing;
     B754_finite b _ _ ->
      case b of {
       Prelude.True -> Prelude.Just Datatypes.Gt;
       Prelude.False -> Prelude.Just Datatypes.Lt}};
   B754_infinity b ->
    case b of {
     Prelude.True ->
      case f2 of {
       B754_infinity b0 ->
        case b0 of {
         Prelude.True -> Prelude.Just Datatypes.Eq;
         Prelude.False -> Prelude.Just Datatypes.Lt};
       B754_nan _ _ -> Prelude.Nothing;
       _ -> Prelude.Just Datatypes.Lt};
     Prelude.False ->
      case f2 of {
       B754_infinity b0 ->
        case b0 of {
         Prelude.True -> Prelude.Just Datatypes.Gt;
         Prelude.False -> Prelude.Just Datatypes.Eq};
       B754_nan _ _ -> Prelude.Nothing;
       _ -> Prelude.Just Datatypes.Gt}};
   B754_nan _ _ -> Prelude.Nothing;
   B754_finite s1 m1 e1 ->
    case s1 of {
     Prelude.True ->
      case f2 of {
       B754_zero _ -> Prelude.Just Datatypes.Lt;
       B754_infinity b ->
        case b of {
         Prelude.True -> Prelude.Just Datatypes.Gt;
         Prelude.False -> Prelude.Just Datatypes.Lt};
       B754_nan _ _ -> Prelude.Nothing;
       B754_finite s2 m2 e2 ->
        case s1 of {
         Prelude.True ->
          case s2 of {
           Prelude.True ->
            case BinInt._Z__compare e1 e2 of {
             Datatypes.Eq -> Prelude.Just
              (Datatypes.coq_CompOpp (BinPos._Pos__compare_cont Datatypes.Eq m1 m2));
             Datatypes.Lt -> Prelude.Just Datatypes.Gt;
             Datatypes.Gt -> Prelude.Just Datatypes.Lt};
           Prelude.False -> Prelude.Just Datatypes.Lt};
         Prelude.False ->
          case s2 of {
           Prelude.True -> Prelude.Just Datatypes.Gt;
           Prelude.False ->
            case BinInt._Z__compare e1 e2 of {
             Datatypes.Eq -> Prelude.Just (BinPos._Pos__compare_cont Datatypes.Eq m1 m2);
             x -> Prelude.Just x}}}};
     Prelude.False ->
      case f2 of {
       B754_zero _ -> Prelude.Just Datatypes.Gt;
       B754_infinity b ->
        case b of {
         Prelude.True -> Prelude.Just Datatypes.Gt;
         Prelude.False -> Prelude.Just Datatypes.Lt};
       B754_nan _ _ -> Prelude.Nothing;
       B754_finite s2 m2 e2 ->
        case s1 of {
         Prelude.True ->
          case s2 of {
           Prelude.True ->
            case BinInt._Z__compare e1 e2 of {
             Datatypes.Eq -> Prelude.Just
              (Datatypes.coq_CompOpp (BinPos._Pos__compare_cont Datatypes.Eq m1 m2));
             Datatypes.Lt -> Prelude.Just Datatypes.Gt;
             Datatypes.Gt -> Prelude.Just Datatypes.Lt};
           Prelude.False -> Prelude.Just Datatypes.Lt};
         Prelude.False ->
          case s2 of {
           Prelude.True -> Prelude.Just Datatypes.Gt;
           Prelude.False ->
            case BinInt._Z__compare e1 e2 of {
             Datatypes.Eq -> Prelude.Just (BinPos._Pos__compare_cont Datatypes.Eq m1 m2);
             x -> Prelude.Just x}}}}}}

data Coq_shr_record =
   Build_shr_record Prelude.Integer Prelude.Bool Prelude.Bool

shr_m :: Coq_shr_record -> Prelude.Integer
shr_m s =
  case s of {
   Build_shr_record shr_m0 _ _ -> shr_m0}

shr_1 :: Coq_shr_record -> Coq_shr_record
shr_1 mrs =
  case mrs of {
   Build_shr_record m r s ->
    let {s0 = (Prelude.||) r s} in
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Build_shr_record 0 Prelude.False s0)
      (\p0 ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\p -> Build_shr_record ((\x -> x) p) Prelude.True s0)
        (\p -> Build_shr_record ((\x -> x) p) Prelude.False s0)
        (\_ -> Build_shr_record 0 Prelude.True s0)
        p0)
      (\p0 ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\p -> Build_shr_record (Prelude.negate p) Prelude.True s0)
        (\p -> Build_shr_record (Prelude.negate p) Prelude.False s0)
        (\_ -> Build_shr_record 0 Prelude.True s0)
        p0)
      m}

loc_of_shr_record :: Coq_shr_record -> Fcalc_bracket.Coq_location
loc_of_shr_record mrs =
  case mrs of {
   Build_shr_record _ shr_r shr_s ->
    case shr_r of {
     Prelude.True ->
      case shr_s of {
       Prelude.True -> Fcalc_bracket.Coq_loc_Inexact Datatypes.Gt;
       Prelude.False -> Fcalc_bracket.Coq_loc_Inexact Datatypes.Eq};
     Prelude.False ->
      case shr_s of {
       Prelude.True -> Fcalc_bracket.Coq_loc_Inexact Datatypes.Lt;
       Prelude.False -> Fcalc_bracket.Coq_loc_Exact}}}

shr_record_of_loc :: Prelude.Integer -> Fcalc_bracket.Coq_location -> Coq_shr_record
shr_record_of_loc m l =
  case l of {
   Fcalc_bracket.Coq_loc_Exact -> Build_shr_record m Prelude.False Prelude.False;
   Fcalc_bracket.Coq_loc_Inexact c ->
    case c of {
     Datatypes.Eq -> Build_shr_record m Prelude.True Prelude.False;
     Datatypes.Lt -> Build_shr_record m Prelude.False Prelude.True;
     Datatypes.Gt -> Build_shr_record m Prelude.True Prelude.True}}

shr :: Coq_shr_record -> Prelude.Integer -> Prelude.Integer -> (,) Coq_shr_record Prelude.Integer
shr mrs e n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) mrs e)
    (\p -> (,) (Fcore_Zaux.iter_pos shr_1 p mrs) ((Prelude.+) e n))
    (\_ -> (,) mrs e)
    n

shr_fexp :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
            Fcalc_bracket.Coq_location -> (,) Coq_shr_record Prelude.Integer
shr_fexp prec emax =
  let {emin = (Prelude.-) ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax) prec}
  in
  let {fexp = Fcore_FLT.coq_FLT_exp emin prec} in
  (\m e l ->
  shr (shr_record_of_loc m l) e ((Prelude.-) (fexp ((Prelude.+) (Fcore_digits.coq_Zdigits2 m) e)) e))

data Coq_mode =
   Coq_mode_NE
 | Coq_mode_ZR
 | Coq_mode_DN
 | Coq_mode_UP
 | Coq_mode_NA

choice_mode :: Coq_mode -> Prelude.Bool -> Prelude.Integer -> Fcalc_bracket.Coq_location ->
               Prelude.Integer
choice_mode m sx mx lx =
  case m of {
   Coq_mode_NE ->
    Fcalc_round.cond_incr (Fcalc_round.round_N (Prelude.not (Fcore_Zaux.coq_Zeven mx)) lx) mx;
   Coq_mode_ZR -> mx;
   Coq_mode_DN -> Fcalc_round.cond_incr (Fcalc_round.round_sign_DN sx lx) mx;
   Coq_mode_UP -> Fcalc_round.cond_incr (Fcalc_round.round_sign_UP sx lx) mx;
   Coq_mode_NA -> Fcalc_round.cond_incr (Fcalc_round.round_N Prelude.True lx) mx}

overflow_to_inf :: Coq_mode -> Prelude.Bool -> Prelude.Bool
overflow_to_inf m s =
  case m of {
   Coq_mode_ZR -> Prelude.False;
   Coq_mode_DN -> s;
   Coq_mode_UP -> Prelude.not s;
   _ -> Prelude.True}

binary_overflow :: Prelude.Integer -> Prelude.Integer -> Coq_mode -> Prelude.Bool -> Coq_full_float
binary_overflow prec emax m s =
  case overflow_to_inf m s of {
   Prelude.True -> F754_infinity s;
   Prelude.False -> F754_finite s
    ((\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
       (\_ -> 1)
       (\p -> p)
       (\_ ->
       1)
       ((Prelude.-) (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) prec) ((\x -> x) 1)))
    ((Prelude.-) emax prec)}

binary_round_aux :: Prelude.Integer -> Prelude.Integer -> Coq_mode -> Prelude.Bool -> Prelude.Integer
                    -> Prelude.Integer -> Fcalc_bracket.Coq_location -> Coq_full_float
binary_round_aux prec emax mode sx mx ex lx =
  case shr_fexp prec emax ((\x -> x) mx) ex lx of {
   (,) mrs' e' ->
    case shr_fexp prec emax (choice_mode mode sx (shr_m mrs') (loc_of_shr_record mrs')) e'
           Fcalc_bracket.Coq_loc_Exact of {
     (,) mrs'' e'' ->
      (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
        (\_ -> F754_zero sx)
        (\m ->
        case BinInt._Z__leb e'' ((Prelude.-) emax prec) of {
         Prelude.True -> F754_finite sx m e'';
         Prelude.False -> binary_overflow prec emax mode sx})
        (\_ -> F754_nan Prelude.False 1)
        (shr_m mrs'')}}

coq_Bmult :: Prelude.Integer -> Prelude.Integer -> (Coq_binary_float -> Coq_binary_float -> (,)
             Prelude.Bool Coq_nan_pl) -> Coq_mode -> Coq_binary_float -> Coq_binary_float ->
             Coq_binary_float
coq_Bmult prec emax mult_nan m x y =
  let {f = \pl -> B754_nan (fst pl) (snd pl)} in
  case x of {
   B754_zero sx ->
    case y of {
     B754_zero sy -> B754_zero (Datatypes.xorb sx sy);
     B754_finite sy _ _ -> B754_zero (Datatypes.xorb sx sy);
     _ -> f (mult_nan x y)};
   B754_infinity sx ->
    case y of {
     B754_infinity sy -> B754_infinity (Datatypes.xorb sx sy);
     B754_finite sy _ _ -> B754_infinity (Datatypes.xorb sx sy);
     _ -> f (mult_nan x y)};
   B754_nan _ _ -> f (mult_nan x y);
   B754_finite sx mx ex ->
    case y of {
     B754_zero sy -> B754_zero (Datatypes.xorb sx sy);
     B754_infinity sy -> B754_infinity (Datatypes.xorb sx sy);
     B754_nan _ _ -> f (mult_nan x y);
     B754_finite sy my ey ->
      coq_FF2B prec emax
        (binary_round_aux prec emax m (Datatypes.xorb sx sy) (BinPos._Pos__mul mx my)
          ((Prelude.+) ex ey) Fcalc_bracket.Coq_loc_Exact)}}

shl_align :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
             Prelude.Integer
shl_align mx ex ex' =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (,) mx ex)
    (\_ -> (,) mx ex)
    (\d -> (,) (Zpower.shift_pos d mx) ex')
    ((Prelude.-) ex' ex)

shl_align_fexp :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> (,)
                  Prelude.Integer Prelude.Integer
shl_align_fexp prec emax =
  let {emin = (Prelude.-) ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax) prec}
  in
  let {fexp = Fcore_FLT.coq_FLT_exp emin prec} in
  (\mx ex -> shl_align mx ex (fexp ((Prelude.+) ((\x -> x) (Fcore_digits.digits2_pos mx)) ex)))

binary_round :: Prelude.Integer -> Prelude.Integer -> Coq_mode -> Prelude.Bool -> Prelude.Integer ->
                Prelude.Integer -> Coq_full_float
binary_round prec emax m sx mx ex =
  case shl_align_fexp prec emax mx ex of {
   (,) mz ez -> binary_round_aux prec emax m sx mz ez Fcalc_bracket.Coq_loc_Exact}

binary_normalize :: Prelude.Integer -> Prelude.Integer -> Coq_mode -> Prelude.Integer ->
                    Prelude.Integer -> Prelude.Bool -> Coq_binary_float
binary_normalize prec emax mode m e szero =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> B754_zero szero)
    (\m0 -> coq_FF2B prec emax (binary_round prec emax mode Prelude.False m0 e))
    (\m0 -> coq_FF2B prec emax (binary_round prec emax mode Prelude.True m0 e))
    m

coq_Bplus :: Prelude.Integer -> Prelude.Integer -> (Coq_binary_float -> Coq_binary_float -> (,)
             Prelude.Bool Coq_nan_pl) -> Coq_mode -> Coq_binary_float -> Coq_binary_float ->
             Coq_binary_float
coq_Bplus prec emax plus_nan m x y =
  let {f = \pl -> B754_nan (fst pl) (snd pl)} in
  case x of {
   B754_zero sx ->
    case y of {
     B754_zero sy ->
      case Bool.eqb sx sy of {
       Prelude.True -> x;
       Prelude.False ->
        case m of {
         Coq_mode_DN -> B754_zero Prelude.True;
         _ -> B754_zero Prelude.False}};
     B754_nan _ _ -> f (plus_nan x y);
     _ -> y};
   B754_infinity sx ->
    case y of {
     B754_infinity sy ->
      case Bool.eqb sx sy of {
       Prelude.True -> x;
       Prelude.False -> f (plus_nan x y)};
     B754_nan _ _ -> f (plus_nan x y);
     _ -> x};
   B754_nan _ _ -> f (plus_nan x y);
   B754_finite sx mx ex ->
    case y of {
     B754_zero _ -> x;
     B754_infinity _ -> y;
     B754_nan _ _ -> f (plus_nan x y);
     B754_finite sy my ey ->
      let {ez = Prelude.min ex ey} in
      binary_normalize prec emax m
        ((Prelude.+) (Fcore_Zaux.cond_Zopp sx ((\x -> x) (fst (shl_align mx ex ez))))
          (Fcore_Zaux.cond_Zopp sy ((\x -> x) (fst (shl_align my ey ez))))) ez
        (case m of {
          Coq_mode_DN -> Prelude.True;
          _ -> Prelude.False})}}

coq_Bminus :: Prelude.Integer -> Prelude.Integer -> (Coq_binary_float -> Coq_binary_float -> (,)
              Prelude.Bool Coq_nan_pl) -> Coq_mode -> Coq_binary_float -> Coq_binary_float ->
              Coq_binary_float
coq_Bminus prec emax minus_nan m x y =
  coq_Bplus prec emax minus_nan m x (coq_Bopp prec emax (\x0 x1 -> (,) x0 x1) y)

coq_Fdiv_core_binary :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                        Prelude.Integer -> (,) ((,) Prelude.Integer Prelude.Integer)
                        Fcalc_bracket.Coq_location
coq_Fdiv_core_binary prec m1 e1 m2 e2 =
  let {d1 = Fcore_digits.coq_Zdigits2 m1} in
  let {d2 = Fcore_digits.coq_Zdigits2 m2} in
  let {e = (Prelude.-) e1 e2} in
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ ->
    case Fcore_Zaux.coq_Zfast_div_eucl m1 m2 of {
     (,) q r -> (,) ((,) q e) (Fcalc_bracket.new_location m2 r Fcalc_bracket.Coq_loc_Exact)})
    (\p ->
    let {m = BinInt._Z__shiftl m1 ((\x -> x) p)} in
    let {e' = (Prelude.+) e (Prelude.negate p)} in
    case Fcore_Zaux.coq_Zfast_div_eucl m m2 of {
     (,) q r -> (,) ((,) q e') (Fcalc_bracket.new_location m2 r Fcalc_bracket.Coq_loc_Exact)})
    (\_ ->
    case Fcore_Zaux.coq_Zfast_div_eucl m1 m2 of {
     (,) q r -> (,) ((,) q e) (Fcalc_bracket.new_location m2 r Fcalc_bracket.Coq_loc_Exact)})
    ((Prelude.-) ((Prelude.+) d2 prec) d1)

coq_Bdiv :: Prelude.Integer -> Prelude.Integer -> (Coq_binary_float -> Coq_binary_float -> (,)
            Prelude.Bool Coq_nan_pl) -> Coq_mode -> Coq_binary_float -> Coq_binary_float ->
            Coq_binary_float
coq_Bdiv prec emax div_nan m x y =
  let {f = \pl -> B754_nan (fst pl) (snd pl)} in
  case x of {
   B754_zero sx ->
    case y of {
     B754_infinity sy -> B754_zero (Datatypes.xorb sx sy);
     B754_finite sy _ _ -> B754_zero (Datatypes.xorb sx sy);
     _ -> f (div_nan x y)};
   B754_infinity sx ->
    case y of {
     B754_zero sy -> B754_infinity (Datatypes.xorb sx sy);
     B754_finite sy _ _ -> B754_infinity (Datatypes.xorb sx sy);
     _ -> f (div_nan x y)};
   B754_nan _ _ -> f (div_nan x y);
   B754_finite sx mx ex ->
    case y of {
     B754_zero sy -> B754_infinity (Datatypes.xorb sx sy);
     B754_infinity sy -> B754_zero (Datatypes.xorb sx sy);
     B754_nan _ _ -> f (div_nan x y);
     B754_finite sy my ey ->
      coq_FF2B prec emax
        (case coq_Fdiv_core_binary prec ((\x -> x) mx) ex ((\x -> x) my) ey of {
          (,) p lz ->
           case p of {
            (,) mz ez ->
             (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
               (\_ -> F754_nan Prelude.False 1)
               (\mz0 -> binary_round_aux prec emax m (Datatypes.xorb sx sy) mz0 ez lz)
               (\_ -> F754_nan Prelude.False 1)
               mz}})}}

coq_Fsqrt_core_binary :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> (,)
                         ((,) Prelude.Integer Prelude.Integer) Fcalc_bracket.Coq_location
coq_Fsqrt_core_binary prec m e =
  let {d = Fcore_digits.coq_Zdigits2 m} in
  let {s = Prelude.max ((Prelude.-) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) prec) d) 0} in
  let {e' = (Prelude.-) e s} in
  case Fcore_Zaux.coq_Zeven e' of {
   Prelude.True ->
    let {
     m' = (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
            (\_ -> m)
            (\p -> BinInt._Z__shiftl m ((\x -> x) p))
            (\_ -> m)
            s}
    in
    case BinInt._Z__sqrtrem m' of {
     (,) q r ->
      let {
       l = case Zbool.coq_Zeq_bool r 0 of {
            Prelude.True -> Fcalc_bracket.Coq_loc_Exact;
            Prelude.False -> Fcalc_bracket.Coq_loc_Inexact
             (case BinInt._Z__leb r q of {
               Prelude.True -> Datatypes.Lt;
               Prelude.False -> Datatypes.Gt})}}
      in
      (,) ((,) q (BinInt._Z__div2 e')) l};
   Prelude.False ->
    let {s' = (Prelude.+) s ((\x -> x) 1)} in
    let {e'' = (Prelude.-) e' ((\x -> x) 1)} in
    let {
     m' = (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
            (\_ -> m)
            (\p -> BinInt._Z__shiftl m ((\x -> x) p))
            (\_ -> m)
            s'}
    in
    case BinInt._Z__sqrtrem m' of {
     (,) q r ->
      let {
       l = case Zbool.coq_Zeq_bool r 0 of {
            Prelude.True -> Fcalc_bracket.Coq_loc_Exact;
            Prelude.False -> Fcalc_bracket.Coq_loc_Inexact
             (case BinInt._Z__leb r q of {
               Prelude.True -> Datatypes.Lt;
               Prelude.False -> Datatypes.Gt})}}
      in
      (,) ((,) q (BinInt._Z__div2 e'')) l}}

coq_Bsqrt :: Prelude.Integer -> Prelude.Integer -> (Coq_binary_float -> (,) Prelude.Bool Coq_nan_pl)
             -> Coq_mode -> Coq_binary_float -> Coq_binary_float
coq_Bsqrt prec emax sqrt_nan m x =
  let {f = \pl -> B754_nan (fst pl) (snd pl)} in
  case x of {
   B754_zero _ -> x;
   B754_infinity b -> case b of {
                       Prelude.True -> f (sqrt_nan x);
                       Prelude.False -> x};
   B754_nan _ _ -> f (sqrt_nan x);
   B754_finite sx mx ex ->
    case sx of {
     Prelude.True -> f (sqrt_nan x);
     Prelude.False ->
      coq_FF2B prec emax
        (case coq_Fsqrt_core_binary prec ((\x -> x) mx) ex of {
          (,) p lz ->
           case p of {
            (,) mz ez ->
             (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
               (\_ -> F754_nan Prelude.False 1)
               (\mz0 -> binary_round_aux prec emax m Prelude.False mz0 ez lz)
               (\_ -> F754_nan Prelude.False 1)
               mz}})}}

