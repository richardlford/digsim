open BinNums

type stateVar =
| SvT
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

(** val state_var_eq : stateVar -> stateVar -> bool **)

let state_var_eq r1 r2 =
  match r1 with
  | SvT -> (match r2 with
            | SvT -> true
            | _ -> false)
  | SvX -> (match r2 with
            | SvX -> true
            | _ -> false)
  | SvXD -> (match r2 with
             | SvXD -> true
             | _ -> false)
  | SvXDD -> (match r2 with
              | SvXDD -> true
              | _ -> false)
  | SvCOEFF_OF_REST -> (match r2 with
                        | SvCOEFF_OF_REST -> true
                        | _ -> false)
  | SvGRAVITY -> (match r2 with
                  | SvGRAVITY -> true
                  | _ -> false)
  | SvT_STOP -> (match r2 with
                 | SvT_STOP -> true
                 | _ -> false)
  | SvDT -> (match r2 with
             | SvDT -> true
             | _ -> false)
  | SvDT_MAX -> (match r2 with
                 | SvDT_MAX -> true
                 | _ -> false)
  | SvDT_MIN -> (match r2 with
                 | SvDT_MIN -> true
                 | _ -> false)
  | SvDT_PRINT -> (match r2 with
                   | SvDT_PRINT -> true
                   | _ -> false)

(** val svIndex : stateVar -> positive **)

let svIndex = function
| SvT -> Coq_xH
| SvX -> Coq_xO Coq_xH
| SvXD -> Coq_xI Coq_xH
| SvXDD -> Coq_xO (Coq_xO Coq_xH)
| SvCOEFF_OF_REST -> Coq_xI (Coq_xO Coq_xH)
| SvGRAVITY -> Coq_xO (Coq_xI Coq_xH)
| SvT_STOP -> Coq_xI (Coq_xI Coq_xH)
| SvDT -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| SvDT_MAX -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| SvDT_MIN -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| SvDT_PRINT -> Coq_xI (Coq_xI (Coq_xO Coq_xH))

(** val svStrList : (stateVar * char list) list **)

let svStrList =
  (SvT, ('T'::[])) :: ((SvX, ('X'::[])) :: ((SvXD,
    ('X'::('D'::[]))) :: ((SvXDD,
    ('X'::('D'::('D'::[])))) :: ((SvCOEFF_OF_REST,
    ('C'::('O'::('E'::('F'::('F'::('_'::('O'::('F'::('_'::('R'::('E'::('S'::('T'::[])))))))))))))) :: ((SvGRAVITY,
    ('G'::('R'::('A'::('V'::('I'::('T'::('Y'::[])))))))) :: ((SvT_STOP,
    ('T'::('_'::('S'::('T'::('O'::('P'::[]))))))) :: ((SvDT,
    ('D'::('T'::[]))) :: ((SvDT_MAX,
    ('D'::('T'::('_'::('M'::('A'::('X'::[]))))))) :: ((SvDT_MIN,
    ('D'::('T'::('_'::('M'::('I'::('N'::[]))))))) :: ((SvDT_PRINT,
    ('D'::('T'::('_'::('P'::('R'::('I'::('N'::('T'::[]))))))))) :: []))))))))))

(** val driver_defaults_str : (stateVar * char list) list **)

let driver_defaults_str =
  (SvT, ('0'::('.'::('0'::[])))) :: ((SvT_STOP,
    ('0'::('.'::('0'::[])))) :: ((SvDT,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MAX,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MIN,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_PRINT,
    ('0'::('.'::('0'::('1'::[]))))) :: [])))))

(** val model_default_values_str : (stateVar * char list) list **)

let model_default_values_str =
  (SvT, ('0'::('.'::('0'::[])))) :: ((SvX,
    ('1'::('0'::('.'::('0'::[]))))) :: ((SvXD,
    ('0'::('.'::('0'::[])))) :: ((SvXDD,
    ('0'::('.'::('0'::[])))) :: ((SvCOEFF_OF_REST,
    ('0'::('.'::('8'::('0'::[]))))) :: ((SvGRAVITY,
    ('9'::('.'::('8'::('8'::[]))))) :: ((SvT_STOP,
    ('1'::('0'::('.'::('0'::[]))))) :: ((SvDT,
    ('0'::('.'::('0'::('1'::[]))))) :: ((SvDT_MAX,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MIN,
    ('0'::('.'::('0'::('0'::('1'::[])))))) :: ((SvDT_PRINT,
    ('0'::('.'::('0'::('1'::[]))))) :: []))))))))))

(** val modelOutputs : stateVar list **)

let modelOutputs =
  SvT :: (SvX :: (SvXD :: []))
