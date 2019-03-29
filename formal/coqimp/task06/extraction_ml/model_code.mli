open Floats
open Integers
open RecordSet
open Debug_printers
open Driver2
open Float_text_io
open Model_data

val z_terminate_sim_event : eventTy

val sq : float -> float

val log_miss : simTy -> simTy

val z_terminate_sim_event_func : event_function_signature

val model_handlers : (char list * event_function_signature) list

val init_sim : simTy -> simTy

val differential_equations : simTy -> simTy
