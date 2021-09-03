
(* The type of tokens. *)

type token = 
  | Smaller
  | STAR
  | SRPAR
  | SLPAR
  | SLASH
  | SEMICOLON
  | RPAR
  | PRIME
  | PLUS
  | OR
  | Num of (string)
  | MINUS
  | Larger
  | LPAR
  | IN
  | IDENTIFIER of (string)
  | FORALL
  | EXISTS
  | EQUAL
  | EOF
  | COMMA
  | COLON
  | ASSIGN
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.tla_fil)
