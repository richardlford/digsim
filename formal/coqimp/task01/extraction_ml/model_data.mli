open BinNums

type stateVar =
| SvT
| SvX
| SvXD
| SvXDD
| SvT_STOP
| SvDT
| SvDT_MAX
| SvDT_MIN
| SvDT_PRINT
| SvDampingCoefficient
| SvSpringCoefficient
| SvGRAVITY
| SvMass

val state_var_eq : stateVar -> stateVar -> bool

val svIndex : stateVar -> positive

val svStrList : (stateVar * char list) list

val driver_defaults_str : (stateVar * char list) list

val model_default_values_str : (stateVar * char list) list

val modelOutputs : stateVar list
