open BinNums
open Binary
open Extr_ocaml_int64_conv
open BinInt

let ocaml_float_of_coq_float (cqfloat: binary_float) =
  let zbits: coq_Z = Bits.bits_of_b64 cqfloat in
  let int64bits: int64 = int64_of_z zbits in
  Int64.float_of_bits int64bits

let coq_float_of_ocaml_float (mlfloat: Float.t) =
  let int64bits: int64 = Int64.bits_of_float mlfloat in
  let uzbits: coq_Z = z_of_uint64 int64bits in
  Bits.b64_of_bits uzbits

let coq_fun (mlfun: Float.t -> Float.t) (coqarg: binary_float) =
  let mlarg = ocaml_float_of_coq_float coqarg in
  let mlresult = mlfun mlarg in
  coq_float_of_ocaml_float mlresult

let coq_fun2 (mlfun: Float.t -> Float.t -> Float.t) (coqarg1: binary_float) (coqarg2: binary_float) =
  let mlarg1 = ocaml_float_of_coq_float coqarg1 in
  let mlarg2 = ocaml_float_of_coq_float coqarg2 in
  let mlresult = mlfun mlarg1 mlarg2 in
  coq_float_of_ocaml_float mlresult

let coq_pow = coq_fun2 Float.pow
let coq_sqrt = coq_fun Float.sqrt
let coq_exp = coq_fun Float.exp
let coq_log = coq_fun Float.log
let coq_log10 = coq_fun Float.log10
let coq_expm1 = coq_fun Float.expm1
let coq_log1p = coq_fun Float.log1p
let coq_cos = coq_fun Float.cos
let coq_sin = coq_fun Float.sin
let coq_tan = coq_fun Float.tan
let coq_acos = coq_fun Float.acos
let coq_asin = coq_fun Float.asin
let coq_atan = coq_fun Float.atan
let coq_atan2 = coq_fun2 Float.atan2
let coq_hypot = coq_fun2 Float.hypot
let coq_cosh = coq_fun Float.cosh
let coq_sinh = coq_fun Float.sinh 
let coq_tanh = coq_fun Float.tanh
let coq_ceil = coq_fun Float.ceil
let coq_floor = coq_fun Float.floor
let coq_copysign = coq_fun2 Float.copysign
