

    let filename = Sys.argv.(1) in
   let input_str = Tree_builder.read_whole_file filename in 
   let  lexbuf =  Lexing.from_string  input_str in
try   let  tla = Parser.start Lexer.token lexbuf in 
    Tree_builder.translate tla 

  
with
  | Invalid_argument _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " 3 4 5")
  | Failure msg        -> print_endline ("Failure in " ^ msg)
  | Parser.Error       -> print_endline "Parse error"
  | End_of_file        -> print_endline "Parse error: unexpected end of string"  
