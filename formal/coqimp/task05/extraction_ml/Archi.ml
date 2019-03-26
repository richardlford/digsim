open BinNums
open Datatypes
open Fappli_IEEE

(** val default_pl_64 : bool * nan_pl **)

let default_pl_64 =
  (true,
    (let rec f = function
     | O -> Coq_xH
     | S n0 -> Coq_xO (f n0)
     in f (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S
          O)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val choose_binop_pl_64 : bool -> nan_pl -> bool -> nan_pl -> bool **)

let choose_binop_pl_64 _ _ _ _ =
  false
