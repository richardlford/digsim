open Driver_state
open Driver_run
open Format
open Debug_printers
open DebugIO
open Maps
open List
open Extr_ocaml_int64_conv
open Fappli_IEEE
open BinNums
open My_printers

let file_row (ch: out_channel) row =
  ignore (map (fun v -> output_string ch (cl2s v); output_char ch ' ') row);
  output_char ch '\n';;

let file_solution (ch: out_channel) sim =
  ignore (map (fun row -> file_row ch row) sim.solution);;

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

  
