open Str 



let file1 = "test.tla"

 
let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string "---" ; print_list l  ;;


let rec f= fun e1  e2 -> fun e1 -> e2 ;;

let read_whole_file t =
    let ch = open_in t in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch; 
    s;;


 (*print_string (read_whole_file file1);; *)
let s = read_whole_file  file1;; 

(*print_string ( s) ;;*)


let capword = global_replace (regexp " ")  "" s ;;
print_string (capword);; 
