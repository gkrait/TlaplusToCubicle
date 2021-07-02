
%{


%}

%token EOL
%token PRIME "'"  EQUAL"="
%token STR
%token Num
%token LSQUARE_BRACKET "[" RSQUARE_BRACKET "]" 
%token MINUS "-"
%token OPEN "("
%token CLOSE ")"  IN "\\n"  
%token  FALSE   
%token  TRUE 
%token OR  "/\\"
%token AND  "\\/"
%token PLUS "+"    


%token  TWO_PERIODS ".."  LCURLY_BRACKET "{"  RCURLY_BRACKET "}"   COMA "," 

%token LARGER ">" LESS "<" NOTEQUAL "#" NOT "~"  COLON ":" FORALL "\\A" EXISTS "\\E"

%token   ARROW "|->"

%token<string> IDEN1 




%left "+" "-"       /* lowest precedence */



%start main
%type <string * (Phrase.phrase * bool) list> main




%%

main:
|  tf=temporal_formula  EOL {tf}



 temporal_formula:
| p=predicate {p}
| primed_exp {pr}


 predicate:
| p=propositional_exp {p}
|  q= quantifier identifier IN identifier COLON  p=predicate  {q::p}
/* The following production caused a conflict that I  could not solve 
 q=quantifier identifier IN  identifier COLON p=predicate {q::p} */


%inline quantifier:
| e=EXISTS {e}  
| f=FORALL {f}



propositional_exp: 
| si=simple_propositional_exp {si} 
| v1=value op=operator v2=value {v1 op v2}
| NOT p=propositional_exp {p}
| OPEN pr1=propositional_exp  l= logical_junction   pr2=propositional_exp CLOSE {pr1 l pr2} /* I added OPEN CLOSE to remove a conflict */


 logical_junction:
| a=AND {}
| o=OR  {}


 operator:
| ne=NOTEQUAL {ne}
| le=LESS  {le}
| la=LARGER {la}



simple_propositional_exp:
|i=identifier EQUAL  v=value {i::v}
| i=identifier IN fs=finite_set {i::f}




finite_set: 
| n1=nonempty_list(numeral) TWO_PERIODS n2=nonempty_list(numeral) {n1::n2}
| n1=numeral+ TWO_PERIODS i=identifier {n1::i}
| i=identifier  TWO_PERIODS n1=numeral+ {i::n1}
| i=identifier  TWO_PERIODS i2=identifier {i::i2}
| LCURLY_BRACKET v=value  list( coma_value ) RCURLY_BRACKET {v} 


%inline coma_value:
|  COMA va= value  {va} 

value:  
| i=identifier {i} 
| n=nonempty_list(numeral) {n}
| MINUS n=numeral   {n}
| b=boolean {b}
| f=func_tla {f}
| i=identifier LSQUARE_BRACKET v=value RSQUARE_BRACKET {i::v}
| st=STR {st}
| v1=value areth v=value {v}
 

%inline areth:
| p=PLUS {p}
| m=MINUS {m}


 boolean:
|FALSE { efalse }
|TRUE { etrue }


primed_exp:
| i=identifier PRIME  EQUAL v=value {i::v}

identifier:
| i1=IDEN1 {i1}

func_tla:
|  LSQUARE_BRACKET  i1=identifier  IN  i2=identifier ARROW v=value RSQUARE_BRACKET  {v}

numeral:
|n=Num {n}



