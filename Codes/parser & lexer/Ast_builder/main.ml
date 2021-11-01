(* let lexbuf = Lexing.from_channel stdin in *)

(*let chan = open_in "main.in" in 
let  lexbuf = Lexing.from_channel  chan  in  *)



let  lexbuf =  Lexing.from_string 
" VARIABLES A, A2  ;
CONSTANTS Proc ;
TypeOk==      A  \\in { \" L1 \" , \" L2 \" , \" L3 \"} 
         /\\  A2 \\in  {  1  ,  2  ,  3, 4  }
         /\\  A3 \\in { \" L4 \" , \" L5 \" , \" L6 \", \"L7\" }   ; 
Init ==     A=[z \\in Proc |->  \" L1 \" ] 
         /\\ A2 =[z \\in Proc |-> 1 ]
         /\\ A3 =[z \\in Proc |-> \" L4 \" ]  ;  
Next(z)==       ( (A'=[A  EXCEPT ![z]=\" L2 \" ]) /\\  (A[z]= \" L1 \") ) 
           \\/  ( (A2'=[A2  EXCEPT ![z]=A2[z]+1 ]) /\\  (A2[z]< 5)      ) 
           \\/  ( (A3'=[A3  EXCEPT ![z]=\" L5 \" ]) /\\  (A[z]= \" L4 \")      ) 
        ;
"
    in


(*  *)

try   let  tla = Parser.start Lexer.token lexbuf in (* parse input *)
    Tree_builder.translate tla 

  
with
  | Invalid_argument _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " 3 4 5")
  | Failure msg        -> print_endline ("Failure in " ^ msg)
  | Parser.Error       -> print_endline "Parse error"
  | End_of_file        -> print_endline "Parse error: unexpected end of string"
