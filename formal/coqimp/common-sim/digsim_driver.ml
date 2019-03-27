open Driver2
open Driver3
open Format
open Debug_printers
open DebugIO
open Maps
open List
open ExtrOcamlInt64Conv
open Fappli_IEEE
open BinNums

let ocaml_float_of_coq_float (cqfloat: binary_float) =
  let zbits: coq_Z = Fappli_IEEE_bits.bits_of_b64 cqfloat in
  let int64bits: int64 = int64_of_z zbits in
  Int64.float_of_bits int64bits

let coq_float_of_ocaml_float (mlfloat: Float.t) =
  let int64bits: int64 = Int64.bits_of_float mlfloat in
  let zbits: coq_Z = z_of_int64 int64bits in
  Fappli_IEEE_bits.b64_of_bits zbits

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

let cl2s cl = String.init (List.length cl) (fun i -> (List.nth cl i));;
let f2s fval = cl2s (print_float fval);;
let print_sv sv = cl2s (svToStr sv);;

let print_svtree tree =
  printf "@[SvFloatTree@,";
  let kvpairs = PTree.elements tree in
   ignore (map (fun (pos, fval) ->
      let sv = posToStateVar' pos in
      let svstring = print_sv sv in
      let fstr = f2s fval in
      printf "@[<h>(%s ->@ %s)@ @]" svstring fstr)
    kvpairs);
  printf "@]";;

let print_vars sim =
  printf "@[{vars@,"; print_svtree sim.vars; printf "},@]";;

let print_solkeys sim =
  printf "@[{solkeys@,";
  ignore (map (fun sv -> print_string (print_sv sv)) sim.solkeys);
  printf "},@]";;

let print_row row =
  printf "@[";
  ignore (map (fun v -> printf "%s\t" (cl2s v)) row);
  printf "@]@,";;

let file_row (ch: out_channel) row =
  ignore (map (fun v -> output_string ch (cl2s v); output_char ch '\t') row);
  output_char ch '\n';;

let print_solution sim =
  printf "@[<v 2>{solution@,";
  ignore (map (fun row -> print_row row) sim.solution);
  printf "},@]@,";;

let file_solution (ch: out_channel) sim =
  ignore (map (fun row -> file_row ch row) sim.solution);;

let print_event ev =
  printf "@[{Event@,";
  printf "@[key=%s,@ time=%s@]@," (cl2s ev.key) (f2s ev.time);
  printf "}@]@,";;

let print_events sim =
  printf "@[<v 2>{events@,";
  ignore (map (fun ev -> print_event ev) sim.sim_events);
  printf "},@]@,";;

let print_vars sim =
  printf "@[<hov 2>{vars@,"; print_svtree sim.vars; printf "}@],@]";;

let print_log_entry le =
  printf "@[<v 2>{log_entry@,";
  printf "@[caption=%s,@]@ " (cl2s le.le_caption);
  printf "@[{le_vars=";
  ignore (map (fun (sv,fval) -> printf "@[%s -> %s@]@ " (cl2s sv) (cl2s fval)) le.le_vars);
  printf "}@]";
  printf "@[{le_events=";
  ignore (map (fun (val1,val2) -> printf "@[%s @ %s@]@ " (cl2s val1) (cl2s val2)) le.le_events);
  printf "}@]";
  printf "},@]@,";;

let print_log sim =
  printf "@[<v 2>{Log@,";
  ignore (map (fun log_entry -> print_log_entry log_entry) sim.log);
  printf "}@]@,";;

let print_flags sim =
  let fl = sim.flags in
  printf "@[<h>{flags: stop_simulation=%B, end_of_run=%B, evaluate_xd=%B}@]"
    fl.stop_simulation fl.end_of_run fl.evaluate_xd;;

let print_sim sim =
  printf "@[<v 2>{simTY@\n";
  print_vars sim;
  print_solkeys sim;
  print_solution sim;
  print_events sim;
  print_log sim;
  print_flags sim;
  printf "@]}";;

let file_sim sim =
  let ch = open_out "output.dat" in
  file_solution ch sim;
  close_out ch;;

let test =
  print_newline();
  let sim = main () in
  print_sim sim;
  file_sim sim;
  print_newline();
  print_endline "This is a test";;

test;;

  
