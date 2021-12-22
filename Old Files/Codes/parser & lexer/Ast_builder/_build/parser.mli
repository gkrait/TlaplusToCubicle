
(* The type of tokens. *)

type token = 
  | VARs of (string)
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
  | Exclamation
  | EXISTS
  | EXCEPT
  | EQUAL
  | EOF
  | DEFINITION_NAME of (string)
  | CONS of (string)
  | COMMA
  | COLON
  | ASSIGN
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.tla_file)
