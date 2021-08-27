
(* The type of tokens. *)

type token = 
  | Smaller
  | STAR
  | SLASH
  | SEMICOLON
  | RPAR
  | PRIME
  | PLUS
  | OR
  | MINUS
  | Larger
  | LPAR
  | IN
  | IDENTIFIER of (string)
  | FORALL
  | EXISTS
  | EQUAL
  | EOF
  | COLON
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.tla_fil)
