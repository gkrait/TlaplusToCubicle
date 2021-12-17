(* lexer.mll -*- tuareg -*- *)
{
    open Lexing
  open Parser
  let get = Lexing.lexeme

type token_ =
    | BOF                               (* beginning of file *)
    | ID of string                      (* identifiers *)
    | OP of string                      (* operators *)
    | KWD of string                     (* keywords *)
    | NUM of string * string            (* numbers *)
    | STR of string                     (* strings *)
    | PUNCT of string                   (* misc. punctuation *)
    | ST of [`Star | `Plus | `Num of int] * string * int




let eol lexbuf =
    let (start, curr) = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      lexbuf.lex_curr_p <- { curr with
                               pos_lnum = curr.pos_lnum + 1 ;
                               pos_bol  = curr.pos_cnum }


}

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let def = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let newline  = ('\r' | '\n' | "\r\n")
let nu = ['0'-'9'] ['0'-'9']*

(* Helpers *)
  let ne  = "\n"
  let tab   = "\009"
  let cr    = "\013"
  let lf    = "\010"
  let eol   =   cr | lf | cr lf 


let whitesp  = [' ']
let tab      = '\t'


let letter   = ['a'-'z' 'A'-'Z']
let numeral  = ['0'-'9']
let namechar = (letter | numeral | '_')
let name     = namechar* letter namechar*

let keyword = (
  "ASSUME"|"ASSUMPTION"|"AXIOM"|"BOOLEAN"|"CASE"|"CHOOSE"|"CONSTANT"
  |"CONSTANTS"|"BY"|"DEF"|"DEFINE"|"DEFS"|"LAMBDA"|"OBVIOUS"|"ELSE"
  |"EXCEPT"|"EXTENDS"|"IF"|"IN"|"INSTANCE"|"LET"|"HAVE"|"TRUE"|"FALSE"
  |"HIDE"|"PROOF"|"PROVE"|"STATE"|"OMITTED"|"LOCAL"|"MODULE"|"OTHER"
  |"THEN"|"THEOREM"|"UNCHANGED"|"QED"|"RECURSIVE"|"WITNESS"|"STRING"
  |"SUFFICES"|"ACTION"|"LEMMA"|"COROLLARY"|"VARIABLE"|"VARIABLES"|"WITH"
  |"TAKE"
  |"USE"|"PICK"|"NEW"|"TEMPORAL"|"PROPOSITION"|"ONLY")






rule modfile = parse
  | "----" '-'* ' '* "MODULE"
      { [ PUNCT "----"; KWD "MODULE" ] }
  | newline
      { eol lexbuf ; modfile lexbuf }
  | _ { modfile lexbuf }
  | eof { [] }

and token = parse
  (* whitespace *)
  | whitesp            { token lexbuf }
  | tab                 {token lexbuf}
  | newline            { eol lexbuf ; token lexbuf }
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
  | ";"                { SEMICOLON }
  |"=="                  { ASSIGN }
  | nu                   { Num (Lexing.lexeme lexbuf)}
  |"VARIABLES"                  { VARs (Lexing.lexeme lexbuf)}
  |"CONSTANTS"                  { CONS (Lexing.lexeme lexbuf)}
  | "EXCEPT"               {EXCEPT}
  | "UNCHANGED"            {UNCHANGED}
  | "MODULE"               {MODULE}
  | "EXTENDS"             {EXTENDS}
  | "FALSE"               {FALSE}
  | "TRUE"                {TRUE}
  | "CASE"                {CASE}
  | "[]"                  {Square}
  | "["                   { SLPAR }
  | "]"                   { SRPAR }    
  | "|->"                 { ARROW }
  | "~"                   {NOT}
  | ","                  { COMMA }  
  | "!"               {Exclamation}
  | "\""              {QUOTATION}
  | "{"               {LCurly_bra} 
  | "}"               {RCurly_bra} 
  | "->"              {ARROW_set}
  | "#"               {NOT_EQ}
  | id                   { IDENTIFIER (Lexing.lexeme lexbuf)  }












