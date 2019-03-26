open BinInt
open BinNums
open Coqlib
open Floats
open Integers
open List0
open RecordSet
open Driver2
open Float_text_io
open Model_code
open Model_data

val tenTo6 : float

val process_one_event : eventTy -> simTy -> eventTy * simTy

val process_events_helper : eventTy list -> simTy -> eventTy list * simTy

val min_event_time : eventTy list -> float -> float

val process_events : simTy -> simTy

val advance_states : (stateVar * stateVar) list -> float -> simTy -> simTy

val advance_model : simTy -> simTy

val oneStep : simTy -> simTy

val run_sim_loop : coq_Z -> simTy -> simTy

val run_sim : simTy -> simTy

val sim_in : unit -> simTy

val main : unit -> simTy
