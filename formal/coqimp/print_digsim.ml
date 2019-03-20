open Top
open Debug_printers
open DebugIO
open Maps

let char_list_to_string cl = String.init (List.length cl) (fun i -> (List.nth cl i));;

let print_svtree tree =
  Format.printf "@[SvFloatTree@,";
  let kvpairs = PTree.elements tree in
  List.map (fun (pos, fval) ->
      let sv = posToStateVar' pos in
      let svstring = char_list_to_string (svToStr sv) in
      let fstr = char_list_to_string (DebugIO.print_float fval) in
      Format.printf "@[(%s ->@ %s)@]" svstring fstr)
    kvpairs;
  Format.printf "@]";;


(* 

For ocaml top level:

#install_printer my_print_z;;
#install_printer my_print_float;; 
#install_printer my_char_list;;

For ocamldebug:

install_printer My_printers.my_print_z
install_printer My_printers.my_print_float
install_printer My_printers.my_print_char_list

 *)

