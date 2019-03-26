open Floats
open Integers
open RecordSet
open Driver2
open Float_text_io
open Model_data

val fhalf : float

val fone : float

val ftwo : float

val f99 : float

val bounceEvent : eventTy

val flip_xd_at_bounce_event_func : eventTy -> simTy -> simTy * float option

val model_handlers :
  (char list * (eventTy -> simTy -> simTy * float option)) list

val init_sim : simTy -> simTy

val differential_equations : simTy -> simTy
