open Top
open Format
open Debug_printers
open DebugIO
open Maps
open List

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
      printf "@[(%s ->@ %s)@]" svstring fstr)
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

let print_solution sim =
  printf "@[{solution@,";
  ignore (map (fun row -> print_row row) sim.solution);
  printf "},@]@,";;

let print_event ev =
  printf "@[{Event@,";
  printf "@[key=%s,@ time=%s@]@," (cl2s ev.key) (f2s ev.time);
  printf "}@]@,";;

let print_events sim =
  printf "@[{events@,";
  ignore (map (fun ev -> print_event ev) sim.sim_events);
  printf "},@]@,";;

let print_vars sim =
  printf "@[{vars@,"; print_svtree sim.vars; printf "},@]";;

let print_log_entry le =
  printf "@[{log_entry@,";
  printf "@[caption=%s,@]@ " (cl2s le.le_caption);
  printf "@[{le_vars=";
  ignore (map (fun (sv,fval) -> printf "@[%s -> %s@]@ " (cl2s sv) (cl2s fval)) le.le_vars);
  printf "}@]";
  printf "@[{le_events=";
  ignore (map (fun (val1,val2) -> printf "@[%s @ %s@]@ " (cl2s val1) (cl2s val2)) le.le_events);
  printf "}@]";
  printf "},@]@,";;

let print_log sim =
  printf "@[{Log@,";
  ignore (map (fun log_entry -> print_log_entry log_entry) sim.log);
  printf "}@]@,";;

let print_flags sim =
  let fl = sim.flags in
  printf "@[{flags=@,@[stop_simulation=%B,@]@ @[end_of_run=%B,]@ @[evaluate_xd=%B@]@,}@]"
    fl.stop_simulation fl.end_of_run fl.evaluate_xd;;

let print_sim sim =
  printf "@[{simTY@,";
  print_vars sim;
  print_solkeys sim;
  print_solution sim;
  print_events sim;
  print_log sim;
  print_flags sim;
  printf "@]";;

let test _ =
  print_newline();
  let sim = main () in
  print_sim sim;;
