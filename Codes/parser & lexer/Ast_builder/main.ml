(* let lexbuf = Lexing.from_channel stdin in *)

(*let chan = open_in "main.in" in 
let  lexbuf = Lexing.from_channel  chan  in  *)



let  lexbuf =  Lexing.from_string 
" VARIABLES [x1,x2] ;
CONSTANTS [y1 , y2] ; 
A== x=3; 
  "
    in



try
 (* let xval = int_of_string (Sys.argv.(1)) in *)
 (* let yval = int_of_string (Sys.argv.(2)) in *)
 (* let zval = int_of_string (Sys.argv.(3)) in *)
 (* let env  = (xval,yval,zval) in *) 
  let  tla = Parser.start Lexer.token lexbuf in (* parse input *)
    Tree_builder.translate tla

  
with
  | Invalid_argument _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " 3 4 5")
  | Failure msg        -> print_endline ("Failure in " ^ msg)
  | Parser.Error       -> print_endline "Parse error"
  | End_of_file        -> print_endline "Parse error: unexpected end of string"
