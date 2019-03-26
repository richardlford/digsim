open BinInt
open BinNums
open Datatypes
open Decimal
open Fappli_IEEE
open Fappli_IEEE_bits
open Fappli_IEEE_extra
open Floats
open Integers
open Monad
open Nat
open String0

module FloatIO =
 struct
  (** val coq_Z_to_float : coq_Z -> float **)

  let coq_Z_to_float = function
  | Z0 -> Float.zero
  | Zpos x -> Float.from_parsed (Coq_xO (Coq_xI (Coq_xO Coq_xH))) x Z0
  | Zneg x ->
    Float.neg (Float.from_parsed (Coq_xO (Coq_xI (Coq_xO Coq_xH))) x Z0)

  module Details =
   struct
    (** val precision_of_float : coq_Z **)

    let precision_of_float =
      Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))

    (** val log10of2scaled3 : coq_Z **)

    let log10of2scaled3 =
      Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
        Coq_xH))))))))

    (** val strToUintHelper : char list -> uint option **)

    let rec strToUintHelper = function
    | [] -> Some Nil
    | x::x0 ->
      bind (Obj.magic optionM) (strToUintHelper x0) (fun lower ->
        (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
          (fun b b0 b1 b2 b3 b4 b5 b6 ->
          if b
          then if b0
               then if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D7 lower)
                                   else None
                              else None
                    else if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D3 lower)
                                   else None
                              else None
               else if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D5 lower)
                                   else None
                              else None
                    else if b2
                         then if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D9 lower)
                                   else None
                              else None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D1 lower)
                                   else None
                              else None
          else if b0
               then if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D6 lower)
                                   else None
                              else None
                    else if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D2 lower)
                                   else None
                              else None
               else if b1
                    then if b2
                         then None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D4 lower)
                                   else None
                              else None
                    else if b2
                         then if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D8 lower)
                                   else None
                              else None
                         else if b3
                              then if b4
                                   then if b5
                                        then None
                                        else if b6
                                             then None
                                             else Some (D0 lower)
                                   else None
                              else None)
          x)

    (** val strToUint : char list -> uint option **)

    let strToUint s =
      bind (Obj.magic optionM) (strToUintHelper s) (fun x -> Some (unorm x))

    (** val strToIntHelper : char list -> int option **)

    let strToIntHelper s = match s with
    | [] -> Some (Pos (D0 Nil))
    | x::x0 ->
      (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
        (fun b b0 b1 b2 b3 b4 b5 b6 ->
        if b
        then if b0
             then if b1
                  then let sign = false in
                       bind (Obj.magic optionM) (Obj.magic strToUint s)
                         (fun uintVal -> Some
                         (if sign then Neg uintVal else Pos uintVal))
                  else if b2
                       then if b3
                            then let sign = false in
                                 bind (Obj.magic optionM)
                                   (Obj.magic strToUint s) (fun uintVal ->
                                   Some
                                   (if sign then Neg uintVal else Pos uintVal))
                            else if b4
                                 then if b5
                                      then let sign = false in
                                           bind (Obj.magic optionM)
                                             (Obj.magic strToUint s)
                                             (fun uintVal -> Some
                                             (if sign
                                              then Neg uintVal
                                              else Pos uintVal))
                                      else if b6
                                           then let sign = false in
                                                bind (Obj.magic optionM)
                                                  (Obj.magic strToUint s)
                                                  (fun uintVal -> Some
                                                  (if sign
                                                   then Neg uintVal
                                                   else Pos uintVal))
                                           else let sign = false in
                                                bind (Obj.magic optionM)
                                                  (Obj.magic strToUint x0)
                                                  (fun uintVal -> Some
                                                  (if sign
                                                   then Neg uintVal
                                                   else Pos uintVal))
                                 else let sign = false in
                                      bind (Obj.magic optionM)
                                        (Obj.magic strToUint s)
                                        (fun uintVal -> Some
                                        (if sign
                                         then Neg uintVal
                                         else Pos uintVal))
                       else let sign = false in
                            bind (Obj.magic optionM) (Obj.magic strToUint s)
                              (fun uintVal -> Some
                              (if sign then Neg uintVal else Pos uintVal))
             else if b1
                  then if b2
                       then if b3
                            then let sign = false in
                                 bind (Obj.magic optionM)
                                   (Obj.magic strToUint s) (fun uintVal ->
                                   Some
                                   (if sign then Neg uintVal else Pos uintVal))
                            else if b4
                                 then if b5
                                      then let sign = false in
                                           bind (Obj.magic optionM)
                                             (Obj.magic strToUint s)
                                             (fun uintVal -> Some
                                             (if sign
                                              then Neg uintVal
                                              else Pos uintVal))
                                      else if b6
                                           then let sign = false in
                                                bind (Obj.magic optionM)
                                                  (Obj.magic strToUint s)
                                                  (fun uintVal -> Some
                                                  (if sign
                                                   then Neg uintVal
                                                   else Pos uintVal))
                                           else let sign = true in
                                                bind (Obj.magic optionM)
                                                  (Obj.magic strToUint x0)
                                                  (fun uintVal -> Some
                                                  (if sign
                                                   then Neg uintVal
                                                   else Pos uintVal))
                                 else let sign = false in
                                      bind (Obj.magic optionM)
                                        (Obj.magic strToUint s)
                                        (fun uintVal -> Some
                                        (if sign
                                         then Neg uintVal
                                         else Pos uintVal))
                       else let sign = false in
                            bind (Obj.magic optionM) (Obj.magic strToUint s)
                              (fun uintVal -> Some
                              (if sign then Neg uintVal else Pos uintVal))
                  else let sign = false in
                       bind (Obj.magic optionM) (Obj.magic strToUint s)
                         (fun uintVal -> Some
                         (if sign then Neg uintVal else Pos uintVal))
        else let sign = false in
             bind (Obj.magic optionM) (Obj.magic strToUint s) (fun uintVal ->
               Some (if sign then Neg uintVal else Pos uintVal)))
        x

    (** val strToInt : char list -> int option **)

    let strToInt s =
      bind (Obj.magic optionM) (strToIntHelper s) (fun x -> Some (norm x))

    (** val strToZ : char list -> coq_Z option **)

    let strToZ s =
      bind (Obj.magic optionM) (Obj.magic strToInt s) (fun x -> Some
        (Z.of_int x))

    (** val splitAtExponent : char list -> char list * char list **)

    let splitAtExponent s =
      let maybeEpos =
        match index O ('E'::[]) s with
        | Some epos -> Some epos
        | None -> index O ('e'::[]) s
      in
      (match maybeEpos with
       | Some epos ->
         ((substring O epos s),
           (substring (S epos) (sub (sub (length s) epos) (S O)) s))
       | None -> (s, []))

    (** val splitAtPoint : char list -> char list * char list **)

    let splitAtPoint s =
      match index O ('.'::[]) s with
      | Some dpos ->
        ((substring O dpos s),
          (substring (S dpos) (sub (sub (length s) dpos) (S O)) s))
      | None -> (s, [])

    (** val decomposeFloatString :
        char list -> (char list * char list) * char list **)

    let decomposeFloatString s =
      let (frac, exp) = splitAtExponent s in ((splitAtPoint frac), exp)

    (** val strToFloatHelper : char list -> (coq_Z * coq_Z) option **)

    let strToFloatHelper s =
      let (p, exp) = decomposeFloatString s in
      let (intPart, fracPart) = p in
      let mantstring = append intPart fracPart in
      bind (Obj.magic optionM) (Obj.magic strToZ mantstring) (fun zmant ->
        bind (Obj.magic optionM) (Obj.magic strToZ exp) (fun zexp ->
          let adjustedZexp = Z.sub zexp (Z.of_nat (length fracPart)) in
          ret (Obj.magic optionM) (zmant, adjustedZexp)))

    (** val strToFloat : char list -> float option **)

    let strToFloat s =
      match strToFloatHelper s with
      | Some p ->
        let (zmant, adjustedZexp) = p in
        ret (Obj.magic optionM)
          (match zmant with
           | Z0 -> Float.zero
           | Zpos x ->
             Float.from_parsed (Coq_xO (Coq_xI (Coq_xO Coq_xH))) x
               adjustedZexp
           | Zneg x ->
             Float.neg
               (Float.from_parsed (Coq_xO (Coq_xI (Coq_xO Coq_xH))) x
                 adjustedZexp))
      | None -> None

    (** val scale_exp : coq_Z -> coq_Z -> coq_Z **)

    let scale_exp d e =
      Z.sub (Z.sub d (Zpos (Coq_xO Coq_xH)))
        (Z.div
          (Z.mul (Z.sub (Z.add e precision_of_float) (Zpos Coq_xH))
            log10of2scaled3) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
          (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))

    (** val digits : coq_Z -> char list **)

    let digits = function
    | Z0 -> '0'::[]
    | Zpos p ->
      (match p with
       | Coq_xI p0 ->
         (match p0 with
          | Coq_xI p1 ->
            (match p1 with
             | Coq_xI p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 'v'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xH -> 'n'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xH -> 'f'::[])
             | Coq_xO p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 'r'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | Coq_xO p4 ->
                     (match p4 with
                      | Coq_xH -> 'z'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | Coq_xH -> 'j'::[])
                | Coq_xH -> 'b'::[])
             | Coq_xH -> '7'::[])
          | Coq_xO p1 ->
            (match p1 with
             | Coq_xI p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 't'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xH -> 'l'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xH -> 'd'::[])
             | Coq_xO p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 'p'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | Coq_xO p4 ->
                     (match p4 with
                      | Coq_xH -> 'x'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | Coq_xH -> 'h'::[])
                | Coq_xH -> '9'::[])
             | Coq_xH -> '5'::[])
          | Coq_xH -> '3'::[])
       | Coq_xO p0 ->
         (match p0 with
          | Coq_xI p1 ->
            (match p1 with
             | Coq_xI p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 'u'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xH -> 'm'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xH -> 'e'::[])
             | Coq_xO p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 'q'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | Coq_xO p4 ->
                     (match p4 with
                      | Coq_xH -> 'y'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | Coq_xH -> 'i'::[])
                | Coq_xH -> 'a'::[])
             | Coq_xH -> '6'::[])
          | Coq_xO p1 ->
            (match p1 with
             | Coq_xI p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 's'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xH -> 'k'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xH -> 'c'::[])
             | Coq_xO p2 ->
               (match p2 with
                | Coq_xI p3 ->
                  (match p3 with
                   | Coq_xH -> 'o'::[]
                   | _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                | Coq_xO p3 ->
                  (match p3 with
                   | Coq_xI _ ->
                     '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))
                   | Coq_xO p4 ->
                     (match p4 with
                      | Coq_xH -> 'w'::[]
                      | _ ->
                        '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[]))))))))))))))))))))))))))
                   | Coq_xH -> 'g'::[])
                | Coq_xH -> '8'::[])
             | Coq_xH -> '4'::[])
          | Coq_xH -> '2'::[])
       | Coq_xH -> '1'::[])
    | Zneg _ ->
      '!'::(' '::('E'::('r'::('r'::('o'::('r'::('.'::(' '::('M'::('a'::('x'::(' '::('r'::('a'::('d'::('i'::('x'::(' '::('i'::('s'::(' '::('3'::('5'::(' '::('!'::[])))))))))))))))))))))))))

    (** val repeat_string : nat -> char list -> char list **)

    let rec repeat_string n c =
      match n with
      | O -> []
      | S x -> append c (repeat_string x c)

    (** val coq_Z_to_string_base10_aux : nat -> nat -> coq_Z -> char list **)

    let rec coq_Z_to_string_base10_aux min_digits fuel num =
      if Z.ltb num Z0
      then 'R'::('e'::('q'::('u'::('i'::('r'::('e'::(' '::('n'::('o'::('n'::('-'::('n'::('e'::('g'::('a'::('t'::('i'::('v'::('e'::(' '::('n'::('u'::('m'::('.'::[]))))))))))))))))))))))))
      else (match fuel with
            | O ->
              'o'::('u'::('t'::(' '::('o'::('f'::(' '::('f'::('u'::('e'::('l'::[]))))))))))
            | S x ->
              (match num with
               | Z0 -> repeat_string min_digits ('0'::[])
               | _ ->
                 append
                   (coq_Z_to_string_base10_aux (pred min_digits) x
                     (Z.div num (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))
                   (digits
                     (Z.modulo num (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))

    (** val coq_Z_to_string_base10 : nat -> coq_Z -> char list **)

    let coq_Z_to_string_base10 min_digits num =
      if Z.ltb num Z0
      then append ('-'::[])
             (coq_Z_to_string_base10_aux min_digits (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
               (S (S (S (S (S (S (S (S
               O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
               (Z.opp num))
      else if Z.eqb num Z0
           then repeat_string min_digits ('0'::[])
           else coq_Z_to_string_base10_aux min_digits (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S
                  O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                  num

    (** val scaled_float_to_Z : float -> coq_Z -> coq_Z **)

    let scaled_float_to_Z x fdigs =
      match Float.to_long
              (Float.mul x
                (Float.from_parsed (Coq_xO (Coq_xI (Coq_xO Coq_xH))) Coq_xH
                  fdigs)) with
      | Some ii -> Int64.signed ii
      | None -> Z0

    (** val insert_decimal : char list -> nat -> char list **)

    let insert_decimal s fdigs =
      let len = length s in
      let front = sub len fdigs in
      append (substring O front s)
        (append ('.'::[]) (substring front fdigs s))

    (** val float_to_string_unsigned : float -> nat -> char list **)

    let float_to_string_unsigned x fdigs =
      match x with
      | B754_finite (b, _, e) ->
        if b
        then []
        else let digs_after_dec = pred fdigs in
             let scale = scale_exp (Z.of_nat fdigs) e in
             let scaled = scaled_float_to_Z x scale in
             let b10 = coq_Z_to_string_base10 (S O) scaled in
             let lb10 = length b10 in
             let scale' =
               if PeanoNat.Nat.ltb lb10 fdigs
               then Z.add scale (Z.of_nat (sub fdigs lb10))
               else scale
             in
             let b10' =
               if PeanoNat.Nat.ltb lb10 fdigs
               then let scaled' = scaled_float_to_Z x scale' in
                    coq_Z_to_string_base10 fdigs scaled'
               else b10
             in
             let d10 = insert_decimal b10' digs_after_dec in
             let true_exp =
               Z.sub (Z.sub (Z.of_nat fdigs) scale') (Zpos Coq_xH)
             in
             let exp_string = coq_Z_to_string_base10 (S O) true_exp in
             append d10 (append ('e'::[]) exp_string)
      | _ -> []

    (** val float_to_string_unpadded : float -> nat -> char list **)

    let float_to_string_unpadded x fdigs =
      match x with
      | B754_zero b ->
        if b then '-'::('0'::('.'::('0'::[]))) else '0'::('.'::('0'::[]))
      | B754_infinity b ->
        if b then '-'::('i'::('n'::('f'::[]))) else 'i'::('n'::('f'::[]))
      | B754_nan (b, _) ->
        if b then '-'::('n'::('a'::('n'::[]))) else 'n'::('a'::('n'::[]))
      | B754_finite (b, _, _) ->
        if b
        then append ('-'::[]) (float_to_string_unsigned (Float.abs x) fdigs)
        else float_to_string_unsigned x fdigs

    (** val pad_to_width : nat -> char list -> char list **)

    let pad_to_width width s =
      let ls = length s in
      let pads = repeat_string (sub width ls) (' '::[]) in append pads s

    (** val float_to_string : nat -> nat -> float -> char list **)

    let float_to_string width fdigs x =
      pad_to_width width (float_to_string_unpadded x fdigs)
   end

  (** val coq_Z_to_string_base10 : nat -> coq_Z -> char list **)

  let coq_Z_to_string_base10 =
    Details.coq_Z_to_string_base10

  (** val float_to_string : nat -> nat -> float -> char list **)

  let float_to_string =
    Details.float_to_string

  (** val strToFloat : char list -> float option **)

  let strToFloat =
    Details.strToFloat

  (** val strToFloat' : char list -> float **)

  let strToFloat' s =
    match strToFloat s with
    | Some x -> x
    | None -> Float.zero

  (** val coq_ZofFloat : float -> coq_Z **)

  let coq_ZofFloat f =
    match coq_ZofB (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))
            (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
            (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f with
    | Some z -> z
    | None -> Z0

  (** val fhalf_in_floatio : float **)

  let fhalf_in_floatio =
    strToFloat' ('0'::('.'::('5'::[])))

  (** val round : float -> float **)

  let round f =
    let z = coq_ZofFloat (Float.add f fhalf_in_floatio) in coq_Z_to_float z

  (** val sqrt : float -> float **)

  let sqrt arg =
    b64_sqrt Coq_mode_NE arg
 end
