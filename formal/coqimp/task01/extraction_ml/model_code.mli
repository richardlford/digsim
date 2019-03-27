open Floats
open Driver2
open Model_data

val model_handlers : (char list * event_function_signature) list

val init_sim : simTy -> simTy

val differential_equations : simTy -> simTy
