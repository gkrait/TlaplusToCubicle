
%{


%}

%token <string> IDENT
%token <string> STRING
%token <string> NUM

%token EOL
%token PRIME "'"  EQUAL"="
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

%token LARGER ">" LESS "<" NOTEQUAL "/=" NOT "~"  COLON ":" FORALL "\\A" EXISTS "\\E"

%token   ARROW "|->"





%left "+" "-"       /* lowest precedence */



%start main
%type <string * (Phrase.phrase * bool) list> main




%%

main:
|  tf=temporal_formula  EOL {tf}



 temporal_formula:
| p=predicate {p}
| pr=primed_exp {pr}
| OPEN t=temporal_formula  CLOSE {t}
|  t1=OPEN t=temporal_formula  CLOSE  logical_junction t2=temporal_formula {t1 l t2}
|  t1=temporal_formula logical_junction t2=OPEN t=temporal_formula  CLOSE  {t1 l t2}
|  t1=temporal_formula logical_junction t2=temporal_formula {t1 l t2}







 predicate:
| p=propositional_exp {p}
| q= quantifier identifier IN identifier COLON OPEN p=predicate CLOSE   {q::p}


 quantifier:
| e=EXISTS {e}  
| f=FORALL {f}



propositional_exp: 
| si=simple_propositional_exp {si} 
| v1=value op=operator v2=value {v1 op v2}
| NOT p=propositional_exp {p}
/*| OPEN t=propositional_exp CLOSE {t}  */
| OPEN pr1=propositional_exp  l= logical_junction   pr2=propositional_exp CLOSE  {pr1 l pr2} 




  logical_junction:
| a=AND {}
| o=OR  {}


 operator:
| ne=NOTEQUAL {ne}
| le=LESS  {le}
| la=LARGER {la}



simple_propositional_exp:
|i=identifier e=EQUAL  v=value {i :: v}
| id=identifier i=IN fs=finite_set {id i fs}




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
| n=numeral {n} /* put nonempty_list */ 
| MINUS n=numeral   {n}
| b=boolean {b}
| f=func_tla {f}
| i=identifier LSQUARE_BRACKET v=value RSQUARE_BRACKET {v}
| st=STRING {st}
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
| i1=IDENT {i1}

func_tla:
|  LSQUARE_BRACKET  i1=identifier  IN  i2=identifier ARROW v=value RSQUARE_BRACKET  {v}

numeral:
|n=NUM {n}



