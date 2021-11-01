%{ %}

%token EOF
%token PLUS MINUS STAR SLASH EQUAL Larger Smaller OR AND  EXISTS FORALL
%token LPAR RPAR COLON IN PRIME SEMICOLON ASSIGN SLPAR  SRPAR ARROW COMMA  Exclamation EXCEPT
%token QUOTATION  LCurly_bra RCurly_bra
%token <string> IDENTIFIER DEFINITION_NAME
%token <string> Num VARs CONS 


%left OR AND 


%start <Ast.tla_file> start
%%

/* Productions */
start : tla_file EOF       { $1 };



tla_file:
| declaration declaration  tla_file_taile {  Ast.File ( Ast.VARI ($1),  Ast.CONS ($2), $3  ) }

tla_file_taile : 
| definition* {Ast.MulDef ($1)}; /* put as list of defs  */ 
/*|  definition   {Ast.Definition  ($1)  } */



nums :
|  Num                   {[$1]}   
| QUOTATION IDENTIFIER QUOTATION {[$2]}
|  Num COMMA nums    {$1::$3}
|  QUOTATION IDENTIFIER QUOTATION COMMA nums    {$2::$5};

set :
| LCurly_bra  nums RCurly_bra {$2}


declaration:
| VARs  varlist SEMICOLON { $2 }
| CONS  varlist  SEMICOLON   { $2 };
 



definition:
| IDENTIFIER optional_varlist(varlist)  ASSIGN temporal_formula SEMICOLON {Ast.Statment (Ast.DEFIN ( $1), $2 , Ast.ASSIG , Ast.Stat $4, Ast.NEWL ) }
| IDENTIFIER optional_varlist(varlist)  ASSIGN  expr SEMICOLON {Ast.Value (Ast.DEFIN ( $1) ,$2,Ast.ASSIG , Ast.Expr $4 , Ast.NEWL ) }



/*
definition:
| DEFINITION_NAME optional_varlist( varlist)  ASSIGN state_function SEMICOLON {Ast.Statment (Ast.DEFIN ( $1), $2 , Ast.ASSIG , $4, Ast.NEWL ) }


  state_function:
| temporal_formula {$1}
| expr {$1}
*/



 temporal_formula:
| predicate { Ast.Predec $1}
| primed_eq { $1 }
| temporal_formula   logical_oper temporal_formula  {Ast.Mix ($1,$2,$3)} 
|  LPAR temporal_formula RPAR {$2}



 predicate:
| proposition {Ast.Prop $1 }
| EXISTS IDENTIFIER IN IDENTIFIER COLON LPAR  predicate RPAR   { Ast.Existence (Ast.Exis,Ast.Var $2, Ast.Inclus, Ast.Var $4, Ast.Col, $7 ) }
| FORALL IDENTIFIER IN IDENTIFIER COLON LPAR predicate  RPAR  { Ast.Universal (Ast.Univ,Ast.Var $2, Ast.Inclus, Ast.Var $4, Ast.Col, $7 ) }


/* problem in quantification */


proposition
: expr EQUAL expr      { Ast.Equality ($1,Ast.EQ,$3) }
| expr Larger expr     { Ast.Inequality ($1,Ast.Greater,$3) }
| expr Smaller expr    { Ast.Inequality ($1,Ast.Less,$3) }
| IDENTIFIER IN set { Ast.Inclusion(Ast.Var($1),$3) } 
| LPAR proposition RPAR { $2 }
| proposition logical_oper proposition { Ast.Coposition ($1,$2,$3) };


 


%inline  logical_oper:
| AND  {Ast.Conj}
| OR {Ast.Disjun}


primed_eq:
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL expr { Ast.Prime ( Ast.Var $1, $2 , $5 ) }
| IDENTIFIER optional_varlist(varlist) PRIME  EQUAL func_excep {Ast.Func_except(Ast.Var($1),$2,$5) };



func_excep :
| SLPAR expr EXCEPT Exclamation  SLPAR expr SRPAR EQUAL  expr  SRPAR {Ast.Func_exception($2,$6,$9 ) }

expr:
  |  expr PLUS term    { Ast.Binop ($1,Ast.Add,$3) }
  |  expr MINUS term   { Ast.Binop ($1,Ast.Sub,$3) }
  |  term              { $1 }
  | expr  paranth(expr) { Ast.Func_img ($1,$2)} 

  

term
  :  term STAR factor  { Ast.Binop($1,Ast.Mul,$3) }
  |  term SLASH factor { Ast.Binop($1,Ast.Div,$3) }
  |  factor            { $1 };



factor:
  | IDENTIFIER       {Ast.Var ($1) }
  | IDENTIFIER optional_varlist(varlist)       {Ast.Var ($1) }
  |  Num                {Ast.INT $1}
  |  LPAR expr RPAR    { $2 }  
  | SLPAR IDENTIFIER IN IDENTIFIER ARROW  expr SRPAR {Ast.Func_def (Ast.Var($2),Ast.Var $4 ,$6) } 
  | QUOTATION IDENTIFIER QUOTATION    { Ast.STRING $2 }

  
varlist:   
       | {[]}
       |  IDENTIFIER  {[$1]}
       |  IDENTIFIER COMMA varlist   {$1::$3};
       ;
        

  %inline optional_varlist(x):
| {[]}
| paranth(x) {$1}

  
paranth(x) :
| LPAR  x  RPAR  { $2}
| SLPAR  x  SRPAR  {$2}  


/*  
varlist: term 
            {
         [$1]
            }
        | term TOK_COMMA termlist
            {
         $1::$3
            }
        ;

  term: TOK_IDENT
        {
     Constant($1)
        }
    ;      
*/
