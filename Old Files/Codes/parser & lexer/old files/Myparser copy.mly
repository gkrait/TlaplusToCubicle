%{
open tla_parser;;
open Printf;;

open Expr;;
open Namespace;;
open Phrase;;

open Lexing;;
open Parsezen;;


%}

%token SPACE " "
%token TABE "  "

%token FORALL "\\A"
%token EXISTS "\\E"
%token OR  "/\\"
%token AND  "\\/"
%token EQUAL "="
%token NOTEQUAL "#"
%token COMMA  ","
%token NOT  "~"

%token PRIME "'"
%token IN "\\n"

%token LARGER ">"
%token LESS "<"

%token MINUS "-"
%token PLUS "+"
%token MINUS "-"
%token TWO_PERIODS ".." 

%token OPEN "("
%token CLOSE ")"
%token LCURLY_BRACKET "{"
%token RCURLY_BRACKET "}"
%token EOL
 
%token UNDERSCORE


%nonassoc OPEN LEFT
%nonassoc ALL EXISTS
%left AND OR
%%

main:

|  temporal_formula  EOL {} 
    

let  temporal_formula:= 
| predicate
| primed_exp
| located(
      ~ = temporal_formula; ~ = logical_junction; ~ =  temporal_formula ;<EBinOp>
      )


%inline operator:
| e=EQUAL {e} 
| ne=NOTEQUAL {ne} 
| le=LESS  {le}
| la=LARGER {la}

%inline logical_junction:
| a=AND {a}
| o=OR  {o}

primed_exp:
| i=identifier PRIME  EQUAL v=value {i::v}

 predicate:
| p=propositional_exp {p}
| q=quantifier identifier IN  identifier COLON p=predicate {q::p}
| OPEN q= quantifier identifier IN identifier COLON p=predicate CLOSE {q::p}
| q= quantifier identifier IN identifier COLON p=predicate  {q::p}



let unary_op ==
  | NOT; { OpNeg }

let propositional_exp:= 
| simple_propositional_exp
| located(
      ~ = value; ~ = operator; value ;<ELiteral>
      )
| located( ~ = unary_op; ~ = propositional_exp ; <OpNeg>)
| delimited(NOT ; propositional_exp  ;CLOSE) ;<EUnOp>
| located(
      ~ = propositional_exp; ~ = logical_junction; ~ =  propositional_exp ;<EBinOp>
      )


simple_propositional_exp:
|i=identifier EQUAL  v=value {i::v}
| i=identifier IN fs=finite_set {i::f}


finite_set: 
| LCURLY_BRACKET v=value* RCURLY_BRACKET {v}
| n1=numeral+ TWO_PERIODS n2=numeral+ {n1::n2}
| n1=numeral+ TWO_PERIODS i=identifier {n1::i}
| i=identifier  TWO_PERIODS n1=numeral+ {i::n1}
| i=identifier  TWO_PERIODS i2=identifier {i::i2}

value:  
| i=identifier {i} 
| n=numeral+ {n}
| MINUS n=numeral {n}
| s=string {s}
| b=boolean {b}
| f=func_tla {f}
| i=identifier LSQUARE_BRACKET v=value RSQUARE_BRACKET {i::v}
| i=identifier OPEN v=value CLOSE {i::v}



%inline quantifier:
| e=EXISTS {e}  
| f=FORALL {f}


identifier:
| n=name {n} /*|name - forbidden */


name:
| n1=nameChar* letter n2=nameChar* {n1::n2}

nameChar:
| l=letter {l}
| n=numeral {n}
| u=UNDERSCORE {u}


letter:
 s=STR {s}


numeral:
| n=Num {n}




let located(x) ==
| ~ = x; { { loc = $loc; value = x } }

let delimited(opening, x, closing) ==
  OPEN ; ~ = x ; CLOSE ; <>

