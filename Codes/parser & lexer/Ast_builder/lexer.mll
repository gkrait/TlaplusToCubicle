(* lexer.mll -*- tuareg -*- *)
{
  open Parser
  let get = Lexing.lexeme
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let nu = ['0'-'9'] ['0'-'9']*

(* Helpers *)
  let ne  = "\n"
  let tab   = "\009"
  let cr    = "\013"
  let lf    = "\010"
  let eol   =   cr | lf | cr lf 

(* Tokens *)

rule token = parse
  | eol                  { token lexbuf }
  | (" " | tab)          { token lexbuf }
  | eof                  { EOF }
  | "+"                  { PLUS }
  | "-"                  { MINUS }
  | "*"                  { STAR }
  | "/"                  { SLASH }
  | "("                  { LPAR }
  | ")"                  { RPAR }
  | "="                  { EQUAL }
  | ">"                  { Larger }
  | "<"                  { Smaller }
  | "/\\"                  { AND }
  | "\\/"                 { OR }
  | "\\A"                {  FORALL }
  | "\\E"                { EXISTS  }
  | ":"                  { COLON }
  | "\\in"                  { IN }
  | "'"                 { PRIME }
  | ';'                { SEMICOLON }
  |"=="                  { ASSIGN }
  | nu                   { Num (Lexing.lexeme lexbuf)}
  | id                   { IDENTIFIER (Lexing.lexeme lexbuf)  }
  | "["                   { SLPAR }
  | "]"                   { SRPAR }    
  | "|->"                 { ARROW }
  | ","                  { COMMA }  
  | "VARIABLES"          {VARIABLES (Lexing.lexeme lexbuf)} 
  | "CONSTANTS"          {CONSTANTS (Lexing.lexeme lexbuf)}

