%{
open Lexing
let rec tmp_to_pred(tmp)= match tmp with 
  | Tla_Ast.Predec(pred) -> pred 
  | Tla_Ast.Mix(tm1,log,tm2) -> Tla_Ast.Pred_Comp(tmp_to_pred tm1, log , tmp_to_pred tm2)


let eol lexbuf =
    let (start, curr) = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      lexbuf.lex_curr_p <- { curr with
                               pos_lnum = curr.pos_lnum + 1 ;
                               pos_bol  = curr.pos_cnum }  

 %}

%token EOF
%token PLUS MINUS STAR SLASH EQUAL Larger Smaller OR AND  EXISTS FORALL
%token LPAR RPAR COLON IN PRIME  ASSIGN SLPAR  SRPAR ARROW COMMA  Exclamation EXCEPT
%token QUOTATION  LCurly_bra RCurly_bra ARROW_set UNCHANGED MODULE EXTENDS TRUE FALSE
%token NOT_EQ NOT CASE  Square END IMPLICATION
%token <string> IDENTIFIER 
%token <string> Num VARs CONS 
%token <string> NEWLINE 



%left OR AND IMPLICATION
%nonassoc EXISTS FORALL 
%nonassoc NOT  




%start <Tla_Ast.tla_file> start
%%

/* Productions */
start : tla_file EOF       { $1 };

moudleName:
hyphen_encloser (MODULE, IDENTIFIER) extends?     {} ;



hyphen_encloser(x,y):
| MINUS MINUS MINUS+ x y MINUS MINUS MINUS+ {}
;

tail:
|  END {}
;

extends:
| EXTENDS varlist {}
;

tla_file:
| moudleName declaration declaration  tla_file_taile tail {  Tla_Ast.File ( Tla_Ast.VARI ($2),  Tla_Ast.CONS ($3), $4  ) }
;

tla_file_taile : 
| definition* {Tla_Ast.MulDef ($1)}; 


set_element:
|  Num                   { $1}   
| QUOTATION IDENTIFIER QUOTATION {$2}
| TRUE {"TRUE"}
| FALSE {"FALSE"}
;

set_elements :
| set_element {[$1]}
|  set_element COMMA set_elements    {$1::$3}
;



set : 
| SLPAR expr ARROW_set  LCurly_bra  set_elements RCurly_bra SRPAR {$5}
;


declaration:
| VARs  varlist  { $2 }
| CONS  varlist     { $2 }
;
 



definition:
| IDENTIFIER paranth_optional(varlist)  ASSIGN temporal_formula NEWLINE?   {Tla_Ast.Statment (Tla_Ast.DEFIN ( $1), $2 , Tla_Ast.ASSIG , Tla_Ast.Stat $4, Tla_Ast.NEWL ) }
/*| IDENTIFIER paranth_optional(varlist)  ASSIGN  expr SEMICOLON {Tla_Ast.Value (Tla_Ast.DEFIN ( $1) ,$2,Tla_Ast.ASSIG , Tla_Ast.Expr $4 , Tla_Ast.NEWL ) }
;*/


 temporal_formula:
| simp_temporal_formula {$1}
| LPAR temporal_formula  RPAR {$2}
|  temporal_formula   AND    temporal_formula   {Tla_Ast.Mix ($1,Tla_Ast.Conj,$3)}  
|  temporal_formula   OR    temporal_formula   {Tla_Ast.Mix ($1,Tla_Ast.Disjun,$3)}  
| temporal_formula IMPLICATION temporal_formula {Tla_Ast.Implication($1,$3) }
| NOT temporal_formula {Tla_Ast.Negation $2 } %prec NOT
;



 simp_temporal_formula:
| primed_eq { $1 }
| predicate { Tla_Ast.Predec $1} ;







composed_prop:
| proposition {$1}
| proposition AND composed_prop {Tla_Ast.Coposition (  $1, Tla_Ast.Conj, $3)   }
| proposition OR composed_prop {Tla_Ast.Coposition (  $1, Tla_Ast.Disjun, $3)   }


predicate: 
| proposition {Tla_Ast.Prop $1} 
| EXISTS varlist IN IDENTIFIER COLON   temporal_formula      { 
  let pred = tmp_to_pred $6 in 
  Tla_Ast.Existence (Tla_Ast.Exis, $2,  Tla_Ast.Var $4, pred) } %prec EXISTS
| FORALL varlist IN IDENTIFIER COLON    temporal_formula      { 
let pred = tmp_to_pred $6 in 
  Tla_Ast.Universal (Tla_Ast.Univ, $2, Tla_Ast.Var $4,  pred) 
   } %prec FORALL
;





  proposition:
 expr EQUAL expr      { Tla_Ast.Equality ($1,$3) } 
| expr Larger expr     { Tla_Ast.Inequality ($1,Tla_Ast.Greater,$3) }
| expr Smaller expr    { Tla_Ast.Inequality ($1,Tla_Ast.Less,$3) }
| expr NOT_EQ expr      { Tla_Ast.Not_equal($1,$3) }
| IDENTIFIER IN set { Tla_Ast.Inclusion(Tla_Ast.Var($1),$3) } 
| UNCHANGED varlist  { Tla_Ast.UNCHAN $2 } 
| IDENTIFIER paranth_optional(varlist)    {Tla_Ast.Open_prop( $1, $2) } 
;




primed_eq:
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL expr_without_func { Tla_Ast.Prime ( Tla_Ast.Var $1, $2 , $5 ) }
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL func_excep {Tla_Ast.Func_except(Tla_Ast.Var($1),$2,$5) }
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL SLPAR varlist IN IDENTIFIER case_func  
    { Tla_Ast.CASES ( Tla_Ast.Func_img(Tla_Ast.Var $1, $2) , $6 , $9 ) }  ;



case_func:
|  ARROW CASE  arrow  arrowt+ SRPAR 
    { $3 :: $4 }

func_excep :
| SLPAR expr EXCEPT Exclamation  SLPAR expr SRPAR EQUAL  expr  SRPAR 
    {Tla_Ast.Func_exception($2,$6,$9 ) };


arrow:
| composed_prop ARROW_set expr   {Tla_Ast.Arrow($1, $3)  };

arrowt:
| Square composed_prop ARROW_set expr {Tla_Ast.Arrow($2, $4)} ;


expr:
expr_without_func {$1}
| function_unprim {$1} ;

expr_without_func:  
  |  expr_without_func PLUS term    { Tla_Ast.Binop ($1,Tla_Ast.Add,$3) }
  |  expr_without_func MINUS term   { Tla_Ast.Binop ($1,Tla_Ast.Sub,$3) }
  |  term              { $1 }  ;

function_unprim:
|  SLPAR IDENTIFIER IN IDENTIFIER  ARROW expr SRPAR
          {Tla_Ast.Func_def(Tla_Ast.Var $2, Tla_Ast.Var $4, $6 )}   

term
  :  term STAR factor  { Tla_Ast.Binop($1,Tla_Ast.Mul,$3) }
  |  term SLASH factor { Tla_Ast.Binop($1,Tla_Ast.Div,$3) }
  |  factor            { $1 };



factor:
  |  Num                {Tla_Ast.INT $1}
  | FALSE                 {Tla_Ast.FALSE}
  | TRUE                  {Tla_Ast.TRUE}
  | QUOTATION IDENTIFIER QUOTATION    { Tla_Ast.STRING $2 }
  | IDENTIFIER  optional_varlist(varlist)    {Tla_Ast.Func_img (Tla_Ast.Var $1, $2) };




varlist:   
       |  IDENTIFIER  {[$1]}
       |  IDENTIFIER COMMA varlist   {$1::$3};
       ;
        

 optional_varlist(x):
| {[]}
| s_paranth(x)
| paranth(x) {$1};

  
%inline paranth(x) :
| LPAR  x  RPAR  { $2};

 paranth_optional(x):
| {[]}
| paranth(x) {$1}  ;


 s_paranth(x) :
| SLPAR  x  SRPAR  {$2} ; 

%inline s_paranth_optional(x):
| {[]}
| s_paranth(x) {$1}  
