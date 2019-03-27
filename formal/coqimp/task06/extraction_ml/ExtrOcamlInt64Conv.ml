open BinNums
open Datatypes

(** val int64_of_nat : nat -> int64 **)

let int64_of_nat =
  let rec loop acc = function
  | O -> acc
  | S n0 -> loop ((Int64.add 1L) acc) n0
  in loop Int64.zero

(** val int64_of_pos : positive -> int64 **)

let rec int64_of_pos = function
| Coq_xI p0 -> (Int64.add 1L) ((Int64.mul 2L) (int64_of_pos p0))
| Coq_xO p0 -> (Int64.mul 2L) (int64_of_pos p0)
| Coq_xH -> (Int64.add 1L) Int64.zero

(** val int64_of_z : coq_Z -> int64 **)

let rec int64_of_z = function
| Z0 -> Int64.zero
| Zpos p -> int64_of_pos p
| Zneg p -> Int64.neg (int64_of_pos p)

(** val int64_of_n : coq_N -> int64 **)

let rec int64_of_n = function
| N0 -> Int64.zero
| Npos p -> int64_of_pos p

(** val int64_natlike_rec : 'a1 -> ('a1 -> 'a1) -> int64 -> 'a1 **)

let int64_natlike_rec = fun fO fS ->
 let rec loop acc (i:int64) = if i <= 0L then acc else loop (fS acc) (Int64.sub i 1L)
 in loop fO

(** val nat_of_int64 : int64 -> nat **)

let nat_of_int64 =
  int64_natlike_rec O (fun x -> S x)

(** val int64_poslike_rec :
    'a1 -> ('a1 -> 'a1) -> ('a1 -> 'a1) -> int64 -> 'a1 **)

let int64_poslike_rec = fun f1 f2x f2x1 ->
 let rec loop (i:int64) = if i <= 1L then f1 else
  if (Int64.logand i 1L) = 0L then f2x (loop (Int64.shift_right_logical i 1)) else f2x1 (loop (Int64.shift_right_logical i 1))
 in loop

(** val pos_of_int64 : int64 -> positive **)

let pos_of_int64 =
  int64_poslike_rec Coq_xH (fun x -> Coq_xO x) (fun x -> Coq_xI x)

(** val int64_zlike_case :
    'a1 -> (int64 -> 'a1) -> (int64 -> 'a1) -> int64 -> 'a1 **)

let int64_zlike_case = fun f0 fpos fneg i ->
 if i = 0L then f0 else if i>0L then fpos i else fneg (Int64.neg i)

(** val z_of_int64 : int64 -> coq_Z **)

let z_of_int64 =
  int64_zlike_case Z0 (fun i -> Zpos (pos_of_int64 i)) (fun i -> Zneg
    (pos_of_int64 i))

(** val n_of_int64 : int64 -> coq_N **)

let n_of_int64 =
  int64_zlike_case N0 (fun i -> Npos (pos_of_int64 i)) (fun _ -> N0)
