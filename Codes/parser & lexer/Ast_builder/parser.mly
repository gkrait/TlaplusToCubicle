%{ %}

%token EOF
%token PLUS MINUS STAR SLASH EQUAL Larger Smaller OR AND  EXISTS FORALL
%token LPAR RPAR COLON IN PRIME SEMICOLON ASSIGN SLPAR  SRPAR ARROW COMMA  Exclamation EXCEPT
%token QUOTATION  LCurly_bra RCurly_bra ARROW_set UNCHANGED MODULE EXTENDS TRUE FALSE
%token NOT_EQ NOT
%token <string> IDENTIFIER 
%token <string> Num VARs CONS 


%nonassoc NOT 
%left PLUS
%left EQUAL
%left AND
%left OR



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
| IDENTIFIER optional_varlist(varlist)  ASSIGN temporal_formula SEMICOLON {Ast.Statment (Ast.DEFIN ( $1), $2 , Ast.ASSIG , Ast.Stat $4, Ast.NEWL ) }
| IDENTIFIER optional_varlist(varlist)  ASSIGN  expr SEMICOLON {Ast.Value (Ast.DEFIN ( $1) ,$2,Ast.ASSIG , Ast.Expr $4 , Ast.NEWL ) }
;


 temporal_formula:
| simp_temporal_formula {$1}
|  simp_temporal_formula   logical_oper    temporal_formula   {Ast.Mix ($1,$2,$3)}  
| NOT temporal_formula {Ast.Negation $2 } %prec NOT 
;



 simp_temporal_formula:
| simple_predicate { Ast.Predec $1}
| primed_eq { $1 }
| IDENTIFIER optional_varlist(varlist)    {Ast.Open_temp( $1, $2) }   ;

 predicate:
| simple_predicate {$1}
|  simple_predicate logical_oper  predicate    {Ast.Pred_Comp( $1,$2,$3)  } ;



simple_predicate: 
| simple_prop {Ast.Prop $1 }
| LPAR predicate  RPAR {$2}
| EXISTS varlist IN IDENTIFIER COLON   predicate     { Ast.Existence (Ast.Exis, $2, Ast.Inclus, Ast.Var $4, Ast.Col, $6 ) }
| FORALL varlist IN IDENTIFIER COLON  predicate    { Ast.Universal (Ast.Univ, $2, Ast.Inclus, Ast.Var $4, Ast.Col, $6 ) }
;
/* problem in quantification */


proposition
: simple_prop {$1}
|  simple_prop logical_oper  proposition  { Ast.Coposition ($1,$2,$3) }   ;
 
%inline  logical_oper:
| AND  {Ast.Conj}
| OR {Ast.Disjun}; 

  simple_prop:
 expr EQUAL expr      { Ast.Equality ($1,Ast.EQ,$3) }
| expr Larger expr     { Ast.Inequality ($1,Ast.Greater,$3) }
| expr Smaller expr    { Ast.Inequality ($1,Ast.Less,$3) }
| expr NOT_EQ expr      { Ast.Not_equal($1,$3) }
| IDENTIFIER IN set { Ast.Inclusion(Ast.Var($1),$3) } 
| UNCHANGED varlist  { Ast.UNCHAN $2 } 
| IDENTIFIER optional_varlist(varlist)    {Ast.Open_prop( $1, $2) }   ;




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
  | IDENTIFIER optional_varlist(varlist)       {Ast.Var ($1) }
  |  Num                {Ast.INT $1}
  | FALSE                 {Ast.FALSE}
  | TRUE                  {Ast.TRUE}
  | SLPAR IDENTIFIER IN IDENTIFIER ARROW  expr SRPAR {Ast.Func_def (Ast.Var($2),Ast.Var $4 ,$6) } 
  | QUOTATION IDENTIFIER QUOTATION    { Ast.STRING $2 };




varlist:   
       | {[]}
       |  IDENTIFIER  {[$1]}
       |  IDENTIFIER COMMA varlist   {$1::$3};
       ;
        

 optional_varlist(x):
| {[]}
| paranth(x) {$1};

  
paranth(x) :
| LPAR  x  RPAR  { $2}
| SLPAR  x  SRPAR  {$2} ; 



