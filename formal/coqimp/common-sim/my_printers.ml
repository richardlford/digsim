open Debug_printers
open DebugIO

let char_list_to_string cl = String.init (List.length cl) (fun i -> (List.nth cl i));;
let my_print_z z = Format.print_string (char_list_to_string (print_Z z));;

let my_print_p p = my_print_z (Zpos p);;

let my_print_float x = Format.print_string (char_list_to_string (print_float x));;

let my_print_char_list (cl: char list) = Format.print_string (char_list_to_string cl);;
(*
#install_printer my_print_z;;
#install_printer my_print_p;;
#install_printer my_print_float;;
#install_printer my_char_list;;
 *)
