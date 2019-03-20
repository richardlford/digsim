
type __ = Obj.t

val xorb : bool -> bool -> bool

val negb : bool -> bool

type nat =
| O
| S of nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

type int =
| Pos of uint
| Neg of uint

val nzhead : uint -> uint

val unorm : uint -> uint

val norm : int -> int

val pred : nat -> nat

val sub : nat -> nat -> nat

type ('e, 't) reader = 'e -> 't

val constructor : 'a2 -> ('a1, 'a2) reader

type ('r, 't) setter = ('t -> 't) -> 'r -> 'r
  (* singleton inductive, whose constructor was Build_Setter *)

val set : ('a1 -> 'a2) -> ('a1, 'a2) setter -> ('a2 -> 'a2) -> 'a1 -> 'a1

val eqb : bool -> bool -> bool

module Nat :
 sig
  val leb : nat -> nat -> bool

  val ltb : nat -> nat -> bool
 end

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : positive -> mask

  val sub_mask : positive -> positive -> mask

  val sub_mask_carry : positive -> positive -> mask

  val mul : positive -> positive -> positive

  val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1

  val square : positive -> positive

  val div2 : positive -> positive

  val div2_up : positive -> positive

  val size : positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val eqb : positive -> positive -> bool

  val leb : positive -> positive -> bool

  val sqrtrem_step : (positive -> positive) -> (positive -> positive) -> (positive * mask) -> positive * mask

  val sqrtrem : positive -> positive * mask

  val coq_lor : positive -> positive -> positive

  val of_succ_nat : nat -> positive

  val of_uint_acc : uint -> positive -> positive

  val of_uint : uint -> n

  val eq_dec : positive -> positive -> bool
 end

val rev : 'a1 list -> 'a1 list

module Z :
 sig
  val double : z -> z

  val succ_double : z -> z

  val pred_double : z -> z

  val pos_sub : positive -> positive -> z

  val add : z -> z -> z

  val opp : z -> z

  val succ : z -> z

  val pred : z -> z

  val sub : z -> z -> z

  val mul : z -> z -> z

  val pow_pos : z -> positive -> z

  val pow : z -> z -> z

  val compare : z -> z -> comparison

  val leb : z -> z -> bool

  val ltb : z -> z -> bool

  val eqb : z -> z -> bool

  val max : z -> z -> z

  val min : z -> z -> z

  val of_nat : nat -> z

  val of_N : n -> z

  val of_uint : uint -> z

  val of_int : int -> z

  val pos_div_eucl : positive -> z -> z * z

  val div_eucl : z -> z -> z * z

  val div : z -> z -> z

  val modulo : z -> z -> z

  val div2 : z -> z

  val log2 : z -> z

  val sqrtrem : z -> z * z

  val shiftl : z -> z -> z

  val eq_dec : z -> z -> bool

  val log2_up : z -> z
 end

val z_lt_dec : z -> z -> bool

val z_le_dec : z -> z -> bool

val z_le_gt_dec : z -> z -> bool

val zeq_bool : z -> z -> bool

val append : char list -> char list -> char list

val length : char list -> nat

val substring : nat -> nat -> char list -> char list

val prefix : char list -> char list -> bool

val index : nat -> char list -> char list -> nat option

val shift_nat : nat -> positive -> positive

val shift_pos : positive -> positive -> positive

val two_power_nat : nat -> z

val zeq : z -> z -> bool

val zlt : z -> z -> bool

val zle : z -> z -> bool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

val zeven : z -> bool

type radix = z
  (* singleton inductive, whose constructor was Build_radix *)

val radix_val : radix -> z

val radix2 : radix

val cond_Zopp : bool -> z -> z

val zpos_div_eucl_aux1 : positive -> positive -> z * z

val zpos_div_eucl_aux : positive -> positive -> z * z

val zfast_div_eucl : z -> z -> z * z

val iter_nat : ('a1 -> 'a1) -> nat -> 'a1 -> 'a1

val iter_pos : ('a1 -> 'a1) -> positive -> 'a1 -> 'a1

val fLT_exp : z -> z -> z -> z

val digits2_pos : positive -> positive

val zdigits2 : z -> z

type location =
| Loc_Exact
| Loc_Inexact of comparison

val new_location_even : z -> z -> location -> location

val new_location_odd : z -> z -> location -> location

val new_location : z -> z -> location -> location

val cond_incr : bool -> z -> z

val round_sign_DN : bool -> location -> bool

val round_sign_UP : bool -> location -> bool

val round_N : bool -> location -> bool

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * positive
| F754_finite of bool * positive * z

type nan_pl = positive

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * nan_pl
| B754_finite of bool * positive * z

val fF2B : z -> z -> full_float -> binary_float

val bopp : z -> z -> (bool -> nan_pl -> bool * nan_pl) -> binary_float -> binary_float

val babs : z -> z -> (bool -> nan_pl -> bool * nan_pl) -> binary_float -> binary_float

val bcompare : z -> z -> binary_float -> binary_float -> comparison option

type shr_record = { shr_m : z; shr_r : bool; shr_s : bool }

val shr_m : shr_record -> z

val shr_1 : shr_record -> shr_record

val loc_of_shr_record : shr_record -> location

val shr_record_of_loc : z -> location -> shr_record

val shr : shr_record -> z -> z -> shr_record * z

val shr_fexp : z -> z -> z -> z -> location -> shr_record * z

type mode =
| Mode_NE
| Mode_ZR
| Mode_DN
| Mode_UP
| Mode_NA

val choice_mode : mode -> bool -> z -> location -> z

val overflow_to_inf : mode -> bool -> bool

val binary_overflow : z -> z -> mode -> bool -> full_float

val binary_round_aux : z -> z -> mode -> bool -> positive -> z -> location -> full_float

val bmult :
  z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
  binary_float

val shl_align : positive -> z -> z -> positive * z

val shl_align_fexp : z -> z -> positive -> z -> positive * z

val binary_round : z -> z -> mode -> bool -> positive -> z -> full_float

val binary_normalize : z -> z -> mode -> z -> z -> bool -> binary_float

val bplus :
  z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
  binary_float

val bminus :
  z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
  binary_float

val fdiv_core_binary : z -> z -> z -> z -> z -> (z * z) * location

val bdiv :
  z -> z -> (binary_float -> binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float ->
  binary_float

val fsqrt_core_binary : z -> z -> z -> (z * z) * location

val bsqrt : z -> z -> (binary_float -> bool * nan_pl) -> mode -> binary_float -> binary_float

type binary64 = binary_float

val default_nan_pl64 : bool * nan_pl

val unop_nan_pl64 : binary64 -> bool * nan_pl

val b64_sqrt : mode -> binary_float -> binary_float

val default_pl_64 : bool * nan_pl

val choose_binop_pl_64 : bool -> nan_pl -> bool -> nan_pl -> bool

type comparison0 =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module Wordsize_64 :
 sig
  val wordsize : nat
 end

module Int64 :
 sig
  val wordsize : nat

  val modulus : z

  val half_modulus : z

  val max_signed : z

  val min_signed : z

  type int = z
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> z

  val coq_P_mod_two_p : positive -> nat -> z

  val coq_Z_mod_modulus : z -> z

  val unsigned : int -> z

  val signed : int -> z

  val repr : z -> int
 end

val bofZ : z -> z -> z -> binary_float

val zofB : z -> z -> binary_float -> z option

val zofB_range : z -> z -> binary_float -> z -> z -> z option

val pos_pow : positive -> positive -> positive

val bparse : z -> z -> positive -> positive -> z -> binary_float

type float = binary64

val cmp_of_comparison : comparison0 -> comparison option -> bool

module Float :
 sig
  val transform_quiet_pl : nan_pl -> nan_pl

  val neg_pl : bool -> nan_pl -> bool * nan_pl

  val abs_pl : bool -> nan_pl -> bool * nan_pl

  val binop_pl : binary64 -> binary64 -> bool * nan_pl

  val zero : float

  val neg : float -> float

  val abs : float -> float

  val add : float -> float -> float

  val sub : float -> float -> float

  val mul : float -> float -> float

  val div : float -> float -> float

  val compare : float -> float -> comparison option

  val cmp : comparison0 -> float -> float -> bool

  val to_long : float -> Int64.int option

  val from_parsed : positive -> positive -> z -> float
 end

type 'm monadOps = { bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm); ret : (__ -> __ -> 'm) }

val bind : 'a1 monadOps -> 'a1 -> ('a2 -> 'a1) -> 'a1

val ret : 'a1 monadOps -> 'a2 -> 'a1

type 't monadTransformerOps = { tops : (__ -> __ monadOps -> 't monadOps);
                                lift : (__ -> __ monadOps -> __ -> __ -> 't) }

val tops : 'a1 monadTransformerOps -> 'a2 monadOps -> 'a1 monadOps

val idM : __ monadOps

val monad_of_transformer : 'a1 monadTransformerOps -> 'a1 monadOps

val optionT : __ monadTransformerOps

val optionM : __ option monadOps

module FloatIO :
 sig
  val coq_Z_to_float : z -> float

  module Details :
   sig
    val precision_of_float : z

    val log10of2scaled3 : z

    val strToUintHelper : char list -> uint option

    val strToUint : char list -> uint option

    val strToIntHelper : char list -> int option

    val strToInt : char list -> int option

    val strToZ : char list -> z option

    val splitAtExponent : char list -> char list * char list

    val splitAtPoint : char list -> char list * char list

    val decomposeFloatString : char list -> (char list * char list) * char list

    val strToFloatHelper : char list -> (z * z) option

    val strToFloat : char list -> float option

    val scale_exp : z -> z -> z

    val digits : z -> char list

    val repeat_string : nat -> char list -> char list

    val coq_Z_to_string_base10_aux : nat -> nat -> z -> char list

    val coq_Z_to_string_base10 : nat -> z -> char list

    val scaled_float_to_Z : float -> z -> z

    val insert_decimal : char list -> nat -> char list

    val float_to_string_unsigned : float -> nat -> char list

    val float_to_string_unpadded : float -> nat -> char list

    val pad_to_width : nat -> char list -> char list

    val float_to_string : nat -> nat -> float -> char list
   end

  val coq_Z_to_string_base10 : nat -> z -> char list

  val float_to_string : nat -> nat -> float -> char list

  val strToFloat : char list -> float option

  val strToFloat' : char list -> float
 end

module DebugIO :
 sig
  val width : nat

  val fdigs : nat

  val print_float : float -> char list

  val print_Z : z -> char list
 end

module PTree :
 sig
  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  val empty : 'a1 t

  val get : positive -> 'a1 t -> 'a1 option

  val set : positive -> 'a1 -> 'a1 t -> 'a1 t

  val remove : positive -> 'a1 t -> 'a1 t

  val bempty : 'a1 t -> bool

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val prev_append : positive -> positive -> positive

  val prev : positive -> positive

  val coq_Node' : 'a1 t -> 'a1 option -> 'a1 t -> 'a1 t

  val xcombine_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

  val xcombine_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

  val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val xelements : 'a1 t -> positive -> (positive * 'a1) list -> (positive * 'a1) list

  val elements : 'a1 t -> (positive * 'a1) list
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module ITree :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
 end

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

val state_var_eq : stateVar -> stateVar -> bool

val svIndex : stateVar -> positive

val svStrList : (stateVar * char list) list

module SvIndex :
 sig
  type t = stateVar

  val index : stateVar -> positive

  val eq : stateVar -> stateVar -> bool
 end

module SvTree :
 sig
  type elt = SvIndex.t

  val elt_eq : SvIndex.t -> SvIndex.t -> bool

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val combine : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
 end

type stringSvTreeTy = char list SvTree.t

val emptyStringPTree : char list SvTree.t

val updateStringSvTree : (stateVar * char list) list -> stringSvTreeTy -> stringSvTreeTy

val svToStringTree : stringSvTreeTy

val svToStrOpt : stateVar -> char list option

val svToStr : stateVar -> char list

type floatSvTreeTy = float SvTree.t

val emptyFloatPTree : float SvTree.t

val updateFloatSvTree : (stateVar * float) list -> floatSvTreeTy -> floatSvTreeTy

val stringKeyValToFloatKeyVal : (stateVar * char list) list -> (stateVar * float) list

val driver_defaults_str : (stateVar * char list) list

val driver_defaults : unit -> (stateVar * float) list

val model_default_values_str : (stateVar * char list) list

val model_default_values : unit -> (stateVar * float) list

val state0 : unit -> floatSvTreeTy

val svGetFloat : stateVar -> floatSvTreeTy -> float

val modelOutputs : stateVar list

type flagsTy = { stop_simulation : bool; end_of_run : bool; evaluate_xd : bool }

val stop_simulation : flagsTy -> bool

val end_of_run : flagsTy -> bool

val evaluate_xd : flagsTy -> bool

type eventFuncId =
| EfiStop
| EfiAppendSolution
| EfiTerminateSim
| EfiFlip_Xd_At_Bounce

type eventTy = { key : char list; time : float; func : eventFuncId }

val key : eventTy -> char list

val time : eventTy -> float

val func : eventTy -> eventFuncId

type logEntryTy = { le_caption : char list; le_vars : (char list * char list) list;
                    le_events : (char list * char list) list }

type simTy = { vars : floatSvTreeTy; solkeys : stateVar list; solution : char list list list;
               sim_events : eventTy list; log : logEntryTy list; flags : flagsTy }

val vars : simTy -> floatSvTreeTy

val solkeys : simTy -> stateVar list

val solution : simTy -> char list list list

val sim_events : simTy -> eventTy list

val log : simTy -> logEntryTy list

val flags : simTy -> flagsTy

val log_sim : char list -> simTy -> simTy

val default_flags : flagsTy

val t_ge_tstop_event_func : eventTy -> simTy -> simTy * float option

val make_solution_row_helper : floatSvTreeTy -> stateVar list -> char list list

val make_solution_row : simTy -> simTy

val append_solution_event_func : eventTy -> simTy -> simTy * float option

val terminate_sim_event_func : eventTy -> simTy -> simTy * float option

val driver_default_events : eventTy list

val default_sim : simTy

val default_sim_log : simTy

val f99 : float

val bounceEvent : eventTy

val init_sim : simTy -> simTy

val set_vars : simTy -> floatSvTreeTy -> simTy

val flip_xd_at_bounce_event_func : eventTy -> simTy -> simTy * float option

val handle_event : eventTy -> simTy -> simTy * float option

val set_var : stateVar -> float -> simTy -> simTy

val fhalf : float

val ftwo : float

val epsilon : float

val efi_eqb : eventFuncId -> eventFuncId -> bool

val schedule_event : eventTy list -> eventFuncId -> float -> eventTy list

val sqrt : float -> float

val fone : float

val differential_equations : simTy -> simTy

val zofFloat : float -> z

val round : float -> float

val tenTo6 : float

val process_one_event : eventTy -> simTy -> eventTy * simTy

val process_events_helper : eventTy list -> simTy -> eventTy list * simTy

val min_event_time : eventTy list -> float -> float

val process_events : simTy -> simTy

val advance_states : (stateVar * stateVar) list -> float -> simTy -> simTy

val advance_model : simTy -> simTy

val oneStep : simTy -> simTy

val run_sim_loop : z -> simTy -> simTy

val run_sim : simTy -> simTy

val sim_in : unit -> simTy

val main : unit -> simTy
