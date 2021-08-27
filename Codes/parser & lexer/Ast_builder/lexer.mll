(* lexer.mll -*- tuareg -*- *)
{
  open Parser
  let get = Lexing.lexeme
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* Helpers *)
  let ne  = "\n"
  let tab   = "\009"
  let cr    = "\013"
  let lf    = "\010"
  let eol   =  ne| cr | lf | cr lf 

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
  | "\\/"                { AND }
  | "/\\"                { OR }
  | "@"                {  FORALL }
  | "$"                { EXISTS  }
  | ":"                  { COLON }
  | "^"                  { IN }
  | "["                 { PRIME }
  | ("x" | "y" | "z")    { IDENTIFIER (get lexbuf) }
  | ';'                { SEMICOLON }
  |"=="                  { ASSIGN }
  | id
    { IDENTIFIER (Lexing.lexeme lexbuf) }
 

