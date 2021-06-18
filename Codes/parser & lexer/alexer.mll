(*
 * Modified version of: https://github.com/tlaplus/tlapm/blob/master/src/alexer.mll  :
 * alexer.mll --- lexer
 *
 *
 * Copyright (C) 2008-2010  INRIA and Microsoft Corporation
 *)
{

  open Lexing
  open Pars
  open Tla_parser.Token

  module E = Error
  exception Error of E.t

  let eol lexbuf =
    let (start, curr) = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      lexbuf.lex_curr_p <- { curr with
                               pos_lnum = curr.pos_lnum + 1 ;
                               pos_bol  = curr.pos_cnum }

  let tab_complain lexbuf =
    let e = E.error (Loc.locus_of_position lexbuf.lex_start_p) in
    let e =
      E.err_set_unexpected "TAB character. TLAPS does not handle TAB \
                            characters in source files." e
    in
    raise (Error e)

}

let whitesp  = [' ']
let tab      = '\t'
let newline  = ('\r' | '\n' | "\r\n")

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
  | newline            { eol lexbuf ; token lexbuf }
  | tab                { tab_complain lexbuf }


  (* pragmas *)
  | ("(*{"|"}*)" as prag)
      { [ PUNCT prag ] }

  (* comments *)
  | "\\*"
      { linecom lexbuf }
  | "(*"
      { comment 1 lexbuf }

  (* exceptions *)
  | ("[]" as op)   
      { [ OP op ] }

  (* strict punctuation *)
  | "----" '-'*
      { [ PUNCT "----" ] }
  | "====" '='*
      { [ PUNCT "====" ] }

  | '<' (numeral+ as num) '>' (namechar* as lab) ('.'* as dots)
      { [ ST (`Num (int_of_string num), lab, String.length dots) ] }

  | (","|"."|"_"|"("|")"|"["|"]"|"{"|"}"|"<<"|">>"|"]_"|">>_"|"=="|"!"
    |"@"|":"|"::"|";"|"->"|"<-"|"|->"|"\\A"|"\\E"|'_' as p)
      { [ PUNCT p ] }



