(* let lexbuf = Lexing.from_channel stdin in *)

(*let chan = open_in "main.in" in 
let  lexbuf = Lexing.from_channel  chan  in  *)




   let input_str = Tree_builder.read_whole_file ("input.in") in 
   let  lexbuf =  Lexing.from_string  input_str in
    
    (*

more general  form more than 3 defs 
add Tla header 
use examples to test  *)



try   let  tla = Parser.start Lexer.token lexbuf in (* parse input *)
    Tree_builder.translate tla 

  
with
  | Invalid_argument _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " 3 4 5")
  | Failure msg        -> print_endline ("Failure in " ^ msg)
  | Parser.Error       -> print_endline "Parse error"
  | End_of_file        -> print_endline "Parse error: unexpected end of string"  
