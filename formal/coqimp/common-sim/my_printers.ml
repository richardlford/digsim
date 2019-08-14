open Driver_state
open Driver_run
open Format
open Debug_printers
open DebugIO
open Maps
open List
open Extr_ocaml_int64_conv
open BinNums

let cl2s cl = String.init (List.length cl) (fun i -> (List.nth cl i));;
let f2s fval = cl2s (print_float fval);;
let print_sv sv = cl2s (svToStr sv);;
let vars_per_line = 4;;
let print_svtree tree =
  printf "@[SvFloatTree@,";
  let kvpairs = PTree.elements tree in
  let kvstringpairs =
    map (fun (pos, fval) -> 
        let sv = posToStateVar' pos in
        let svstring = print_sv sv in
        let fstr = f2s fval in
        (svstring, fstr)) kvpairs in
  let sortedkvs = List.sort (fun (x1, y1) (x2, y2) -> compare x1 x2) kvstringpairs in
  printf "@[<h>";
  iteri (fun i (svstring, fstr) ->
      printf "(%-20s, %14s) " svstring fstr;
      if i mod vars_per_line = (vars_per_line - 1) then
        printf "@]@,@[<h>")
    sortedkvs;
  printf "@]@,";;

let print_vars sim =
  printf "@[{vars@,"; print_svtree sim.vars; printf "},@]";;

let print_solkeys sim =
  printf "@[{solkeys@,";
  ignore (map (fun sv -> printf "%s " (print_sv sv)) sim.solkeys);
  printf "},@]";;

let print_row row =
  printf "@[";
  ignore (map (fun v -> printf "%s\t" (cl2s v)) row);
  printf "@]@,";;

let print_solution sim =
  printf "@[<v 2>{solution@,";
  ignore (map (fun row -> print_row row) sim.solution);
  printf "},@]@,";;

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

let print_log_entries sim =
  printf "@[<v 2>{Log@,";
  ignore (map (fun log_entry -> print_log_entry log_entry) sim.log_entries);
  printf "}@]@,";;

let print_flags sim =
  let fl = sim.flags in
  printf "@[<h>{flags: stop_simulation=%B, end_of_run=%B, evaluate_xd=%B}@]"
    fl.stop_simulation fl.end_of_run fl.evaluate_xd;;

let file_svtree (ch: out_channel) tree =
  let kvpairs = PTree.elements tree in
  let kvstringpairs =
    map (fun (pos, fval) -> 
        let sv = posToStateVar' pos in
        let svstring = print_sv sv in
        let fstr = f2s fval in
        (svstring, fstr)) kvpairs in
  let sortedkvs = List.sort (fun (x1, y1) (x2, y2) -> compare x1 x2) kvstringpairs in
  iteri (fun i (svstring, fstr) ->
      Printf.fprintf ch "(%-20s, %14s)\n" svstring fstr)
    sortedkvs;;

let file_vars sim =
  let ch = open_out "vars.dat" in
  file_svtree ch sim.vars;
  close_out ch;;

let print_sim sim =
  printf "@[<v 2>{simTY@\n";
  print_vars sim;
  print_solkeys sim;
  print_solution sim;
  print_events sim;
  print_log_entries sim;
  print_flags sim;
  printf "@]}";
  file_vars sim;;

let my_print_z z = Format.print_string (cl2s (print_Z z));;

let my_print_p p = my_print_z (Zpos p);;

let my_print_float x = Format.print_string (cl2s (print_float x));;

let my_print_char_list (cl: char list) = Format.print_string (cl2s cl);;


(*
#install_printer my_print_z;;
#install_printer my_print_p;;
#install_printer my_print_float;;
#install_printer my_char_list;;
 *)
