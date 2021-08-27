(* let lexbuf = Lexing.from_channel stdin in *)
let lexbuf = Lexing.from_string "zss== x[=y \\/ $ ssx ^ y: x+dy = y-z /\\ xdd2+y > y-z \\/ xss+y < y-z ;
x==y ;
x==y+y ;
z==x >y ;
"  in
try
 (* let xval = int_of_string (Sys.argv.(1)) in *)
 (* let yval = int_of_string (Sys.argv.(2)) in *)
 (* let zval = int_of_string (Sys.argv.(3)) in *)
 (* let env  = (xval,yval,zval) in *) 
  let  tla = Parser.start Lexer.token lexbuf in (* parse input *)
    let  res =Tree_builder.parse_tla tla in 
    print_string " correct"

  
with
  | Invalid_argument _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " 3 4 5")
  | Failure msg        -> print_endline ("Failure in " ^ msg)
  | Parser.Error       -> print_endline "Parse error"
  | End_of_file        -> print_endline "Parse error: unexpected end of string"

