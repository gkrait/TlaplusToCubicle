
(* The type of tokens. *)

type token = 
  | TWO_PERIODS
  | TRUE
  | STR
  | RSQUARE_BRACKET
  | RCURLY_BRACKET
  | PRIME
  | PLUS
  | OR
  | OPEN
  | Num
  | NOTEQUAL
  | NOT
  | MINUS
  | LSQUARE_BRACKET
  | LESS
  | LCURLY_BRACKET
  | LARGER
  | IN
  | IDEN1 of (string)
  | FORALL
  | FALSE
  | EXISTS
  | EQUAL
  | EOL
  | COMA
  | COLON
  | CLOSE
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string * (Phrase.phrase * bool) list)
