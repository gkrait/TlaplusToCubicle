(* let lexbuf = Lexing.from_channel stdin in *)

(*let chan = open_in "main.in" in 
let  lexbuf = Lexing.from_channel  chan  in  *)



let  lexbuf =  Lexing.from_string 
" VARIABLES A ;
CONSTANTS Proc ;
Init == A=[obj \\in Proc |-> 1 ];  
Next== A'=[A  EXCEPT ![t]=2 ] /\\  \\A t \\in Proc : (A[t]<10)   ;
"
    in


(*  A'=[A  EXCEPT ![t]=2 ] /\\   *)

try   let  tla = Parser.start Lexer.token lexbuf in (* parse input *)
    Tree_builder.translate tla 

  
with
  | Invalid_argument _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " 3 4 5")
  | Failure msg        -> print_endline ("Failure in " ^ msg)
  | Parser.Error       -> print_endline "Parse error"
  | End_of_file        -> print_endline "Parse error: unexpected end of string"
