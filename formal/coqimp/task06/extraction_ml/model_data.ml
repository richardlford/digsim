open BinNums

type stateVar =
| SvT
| SvX
| SvXD
| SvXDD
| SvZ
| SvZD
| SvTHETA
| SvTHETA_DOT
| SvTHETA_DOT_CMD
| SvQ_S
| SvQ_S_MEAS
| SvVELOCITY
| SvGUIDANCE_GAIN
| SvTHETA_IC_DG
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
  | SvZ -> (match r2 with
            | SvZ -> true
            | _ -> false)
  | SvZD -> (match r2 with
             | SvZD -> true
             | _ -> false)
  | SvTHETA -> (match r2 with
                | SvTHETA -> true
                | _ -> false)
  | SvTHETA_DOT -> (match r2 with
                    | SvTHETA_DOT -> true
                    | _ -> false)
  | SvTHETA_DOT_CMD -> (match r2 with
                        | SvTHETA_DOT_CMD -> true
                        | _ -> false)
  | SvQ_S -> (match r2 with
              | SvQ_S -> true
              | _ -> false)
  | SvQ_S_MEAS -> (match r2 with
                   | SvQ_S_MEAS -> true
                   | _ -> false)
  | SvVELOCITY -> (match r2 with
                   | SvVELOCITY -> true
                   | _ -> false)
  | SvGUIDANCE_GAIN -> (match r2 with
                        | SvGUIDANCE_GAIN -> true
                        | _ -> false)
  | SvTHETA_IC_DG -> (match r2 with
                      | SvTHETA_IC_DG -> true
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
| SvZ -> Coq_xI (Coq_xO Coq_xH)
| SvZD -> Coq_xO (Coq_xI Coq_xH)
| SvTHETA -> Coq_xI (Coq_xI Coq_xH)
| SvTHETA_DOT -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| SvTHETA_DOT_CMD -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| SvQ_S -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| SvQ_S_MEAS -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| SvVELOCITY -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| SvGUIDANCE_GAIN -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| SvTHETA_IC_DG -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| SvT_STOP -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| SvDT -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| SvDT_MAX -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| SvDT_MIN -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| SvDT_PRINT -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))

(** val svStrList : (stateVar * char list) list **)

let svStrList =
  (SvT, ('T'::[])) :: ((SvX, ('X'::[])) :: ((SvXD,
    ('X'::('D'::[]))) :: ((SvXDD, ('X'::('D'::('D'::[])))) :: ((SvZ,
    ('Z'::[])) :: ((SvZD, ('Z'::('D'::[]))) :: ((SvTHETA,
    ('T'::('H'::('E'::('T'::('A'::[])))))) :: ((SvTHETA_DOT,
    ('T'::('H'::('E'::('T'::('A'::('_'::('D'::('O'::('T'::[])))))))))) :: ((SvTHETA_DOT_CMD,
    ('T'::('H'::('E'::('T'::('A'::('_'::('D'::('O'::('T'::('_'::('C'::('M'::('D'::[])))))))))))))) :: ((SvQ_S,
    ('Q'::('_'::('S'::[])))) :: ((SvQ_S_MEAS,
    ('Q'::('_'::('S'::('_'::('M'::('E'::('A'::('S'::[]))))))))) :: ((SvVELOCITY,
    ('V'::('E'::('L'::('O'::('C'::('I'::('T'::('Y'::[]))))))))) :: ((SvGUIDANCE_GAIN,
    ('G'::('U'::('I'::('D'::('A'::('N'::('C'::('E'::('_'::('G'::('A'::('I'::('N'::[])))))))))))))) :: ((SvTHETA_IC_DG,
    ('T'::('H'::('E'::('T'::('A'::('_'::('I'::('C'::('_'::('D'::('G'::[])))))))))))) :: ((SvT_STOP,
    ('T'::('_'::('S'::('T'::('O'::('P'::[]))))))) :: ((SvDT,
    ('D'::('T'::[]))) :: ((SvDT_MAX,
    ('D'::('T'::('_'::('M'::('A'::('X'::[]))))))) :: ((SvDT_MIN,
    ('D'::('T'::('_'::('M'::('I'::('N'::[]))))))) :: ((SvDT_PRINT,
    ('D'::('T'::('_'::('P'::('R'::('I'::('N'::('T'::[]))))))))) :: []))))))))))))))))))

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
    ('-'::('5'::('0'::('0'::('.'::('0'::[]))))))) :: ((SvXD,
    ('0'::('.'::('0'::[])))) :: ((SvXDD, ('0'::('.'::('0'::[])))) :: ((SvZ,
    ('-'::('1'::('0'::('0'::('.'::('0'::[]))))))) :: ((SvZD,
    ('0'::('.'::('0'::[])))) :: ((SvTHETA,
    ('0'::('.'::('0'::[])))) :: ((SvTHETA_DOT,
    ('0'::('.'::('0'::[])))) :: ((SvTHETA_DOT_CMD,
    ('0'::('.'::('0'::[])))) :: ((SvQ_S,
    ('0'::('.'::('0'::[])))) :: ((SvQ_S_MEAS,
    ('0'::('.'::('0'::[])))) :: ((SvVELOCITY,
    ('1'::('0'::('0'::('.'::('0'::[])))))) :: ((SvGUIDANCE_GAIN,
    ('3'::('.'::('0'::[])))) :: ((SvTHETA_IC_DG,
    ('0'::('.'::('0'::[])))) :: ((SvT_STOP,
    ('1'::('0'::('.'::('0'::[]))))) :: ((SvDT,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MAX,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_MIN,
    ('0'::('.'::('0'::('0'::('5'::[])))))) :: ((SvDT_PRINT,
    ('0'::('.'::('0'::('1'::[]))))) :: []))))))))))))))))))

(** val modelOutputs : stateVar list **)

let modelOutputs =
  SvT :: (SvX :: (SvZ :: (SvTHETA :: (SvXD :: (SvZD :: (SvQ_S :: []))))))

(** val modelPairs : (stateVar * stateVar) list **)

let modelPairs =
  (SvX, SvXD) :: ((SvZ, SvZD) :: ((SvTHETA, SvTHETA_DOT) :: []))
