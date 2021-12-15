%{

let rec tmp_to_pred(tmp)= match tmp with 
  | Ast.Predec(pred) -> pred 
  | Ast.Mix(tm1,log,tm2) -> Ast.Pred_Comp(tmp_to_pred tm1, log , tmp_to_pred tm2)

 %}

%token EOF
%token PLUS MINUS STAR SLASH EQUAL Larger Smaller OR AND  EXISTS FORALL
%token LPAR RPAR COLON IN PRIME SEMICOLON ASSIGN SLPAR  SRPAR ARROW COMMA  Exclamation EXCEPT
%token QUOTATION  LCurly_bra RCurly_bra ARROW_set UNCHANGED MODULE EXTENDS TRUE FALSE
%token NOT_EQ NOT
%token <string> IDENTIFIER 
%token <string> Num VARs CONS 



%left AND
%left OR
%nonassoc EXISTS FORALL
%nonassoc NOT 

%start <Ast.tla_file> start
%%

/* Productions */
start : tla_file EOF       { $1 };

moudleName:
hyphen_encloser (MODULE, IDENTIFIER) extends?     {} ;



hyphen_encloser(x,y):
| MINUS MINUS MINUS+ x y MINUS MINUS MINUS+ {}
;

tail:
|  EQUAL EQUAL EQUAL+ {}
;

extends:
| EXTENDS varlist {}
;

tla_file:
| moudleName declaration declaration  tla_file_taile tail {  Ast.File ( Ast.VARI ($2),  Ast.CONS ($3), $4  ) }
;

tla_file_taile : 
| definition* {Ast.MulDef ($1)}; 


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
| VARs  varlist SEMICOLON { $2 }
| CONS  varlist  SEMICOLON   { $2 }
;
 



definition:
| IDENTIFIER paranth_optional(varlist)  ASSIGN temporal_formula SEMICOLON {Ast.Statment (Ast.DEFIN ( $1), $2 , Ast.ASSIG , Ast.Stat $4, Ast.NEWL ) }
/*| IDENTIFIER paranth_optional(varlist)  ASSIGN  expr SEMICOLON {Ast.Value (Ast.DEFIN ( $1) ,$2,Ast.ASSIG , Ast.Expr $4 , Ast.NEWL ) }
;*/


 temporal_formula:
| simp_temporal_formula {$1}
| LPAR temporal_formula  RPAR {$2}
|  temporal_formula   AND    temporal_formula   {Ast.Mix ($1,Ast.Conj,$3)}  
|  temporal_formula   OR    temporal_formula   {Ast.Mix ($1,Ast.Disjun,$3)}  
| NOT temporal_formula {Ast.Negation $2 } %prec NOT
;



 simp_temporal_formula:
| primed_eq { $1 }
| predicate { Ast.Predec $1};





predicate: 
| proposition {Ast.Prop $1}
| EXISTS varlist IN IDENTIFIER COLON   temporal_formula      { 
  let pred = tmp_to_pred $6 in 
  Ast.Existence (Ast.Exis, $2, Ast.Inclus, Ast.Var $4, Ast.Col, pred) } %prec EXISTS
| FORALL varlist IN IDENTIFIER COLON    temporal_formula      { 
let pred = tmp_to_pred $6 in 
  Ast.Universal (Ast.Univ, $2, Ast.Inclus, Ast.Var $4, Ast.Col, pred) 
   } %prec FORALL
;
/* problem in quantification */




 /*
 logical_oper:
| AND  {Ast.Conj}
| OR {Ast.Disjun}; 
*/
  proposition:
 expr EQUAL expr      { Ast.Equality ($1,Ast.EQ,$3) }
| expr Larger expr     { Ast.Inequality ($1,Ast.Greater,$3) }
| expr Smaller expr    { Ast.Inequality ($1,Ast.Less,$3) }
| expr NOT_EQ expr      { Ast.Not_equal($1,$3) }
| IDENTIFIER IN set { Ast.Inclusion(Ast.Var($1),$3) } 
| UNCHANGED varlist  { Ast.UNCHAN $2 } 
| IDENTIFIER paranth_optional(varlist)    {Ast.Open_prop( $1, $2) } 
;




primed_eq:
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL expr { Ast.Prime ( Ast.Var $1, $2 , $5 ) }
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL func_excep {Ast.Func_except(Ast.Var($1),$2,$5) };



func_excep :
| SLPAR expr EXCEPT Exclamation  SLPAR expr SRPAR EQUAL  expr  SRPAR {Ast.Func_exception($2,$6,$9 ) };

expr:
  |  expr PLUS term    { Ast.Binop ($1,Ast.Add,$3) }
  |  expr MINUS term   { Ast.Binop ($1,Ast.Sub,$3) }
  |  term              { $1 };

  

term
  :  term STAR factor  { Ast.Binop($1,Ast.Mul,$3) }
  |  term SLASH factor { Ast.Binop($1,Ast.Div,$3) }
  |  factor            { $1 };



factor:
  |  Num                {Ast.INT $1}
  | FALSE                 {Ast.FALSE}
  | TRUE                  {Ast.TRUE}
  | SLPAR IDENTIFIER IN IDENTIFIER ARROW  expr SRPAR {Ast.Func_def (Ast.Var($2),Ast.Var $4 ,$6) } 
  | QUOTATION IDENTIFIER QUOTATION    { Ast.STRING $2 }
  | IDENTIFIER  optional_varlist(varlist)       {Ast.Func_img (Ast.Var $1, $2) };




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


