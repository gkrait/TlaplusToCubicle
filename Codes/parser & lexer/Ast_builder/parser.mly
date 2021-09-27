%{ %}

%token EOF
%token PLUS MINUS STAR SLASH EQUAL Larger Smaller OR AND  EXISTS FORALL
%token LPAR RPAR COLON IN PRIME SEMICOLON ASSIGN SLPAR  SRPAR ARROW COMMA  Exclamation EXCEPT
%token <string> IDENTIFIER 
%token <string> Num VARs CONS 





%start <Ast.tla_file> start
%%

/* Productions */
start : tla_file EOF       { $1 };

tla_file:
| declaration  declaration   tla_file_taile { Ast.File ( Ast.VARI ($1),  Ast.CONS ($2), $3  ) }

tla_file_taile : 
| definition+ {Ast.MulDef ($1)} /* put as list of defs  */ 
/*|  definition   {Ast.Definition  ($1)  } */




declaration:
| VARs  varlist SEMICOLON { $2 }
| CONS varlist  SEMICOLON   { $2 }
 



definition:
| IDENTIFIER optional_varlist( varlist)  ASSIGN temporal_formula SEMICOLON {Ast.Statment (Ast.DEFIN ( $1), $2 , Ast.ASSIG , Ast.Stat $4, Ast.NEWL ) }
| IDENTIFIER optional_varlist( varlist)  ASSIGN  expr SEMICOLON {Ast.Value (Ast.DEFIN ( $1) ,$2,Ast.ASSIG , Ast.Expr $4 , Ast.NEWL ) }


/*
definition:
| IDENTIFIER optional_varlist( varlist)  ASSIGN state_function SEMICOLON {Ast.Statment (Ast.DEFIN ( $1), $2 , Ast.ASSIG , $4, Ast.NEWL ) }
*/



%inline optional_varlist(x):
| {[]}
| paranth(x) {$1}

paranth(x) :
| LPAR  x  RPAR  { $2}
| SLPAR  x  SRPAR  {$2}

%inline  state_function:
| expr {$1}
| temporal_formula {$1}


 temporal_formula:
| predicate { Ast.Predec $1}
| primed_eq { $1 }
| temporal_formula   OR temporal_formula  {Ast.Mix ($1,Ast.Disjun,$3)} 
|  temporal_formula  AND temporal_formula   {Ast.Mix ($1,Ast.Conj,$3) }


primed_eq:
| IDENTIFIER optional_varlist( varlist) PRIME  EQUAL expr { Ast.Prime ( Ast.Var $1, $2 , $5 ) }
| IDENTIFIER optional_varlist( varlist)  PRIME  EQUAL func_excep {Ast.Func_except(Ast.Var($1),$2,$5) };

func_excep :
| SLPAR expr EXCEPT Exclamation  SLPAR expr SRPAR EQUAL  expr  SRPAR {Ast.Func_exception($2,$6,$9 ) }

 predicate:
| proposition {Ast.Prop $1 }
| EXISTS IDENTIFIER IN IDENTIFIER COLON  predicate   { Ast.Existence (Ast.Exis,Ast.Var $2, Ast.Inclus, Ast.Var $4, Ast.Col, $6 ) }
| FORALL IDENTIFIER IN IDENTIFIER COLON  predicate   { Ast.Universal (Ast.Univ,Ast.Var $2, Ast.Inclus, Ast.Var $4, Ast.Col, $6 ) }
| predicate OR predicate  { Ast.Pred_Comp ($1,Ast.Disjun,$3)  }
| predicate AND predicate  { Ast.Pred_Comp ($1,Ast.Conj,$3)  }
/* problem in quantification */


proposition
: expr EQUAL expr      { Ast.Equality ($1,Ast.EQ,$3) }
| expr Larger expr     { Ast.Inequality ($1,Ast.Greater,$3) }
| expr Smaller expr    { Ast.Inequality ($1,Ast.Less,$3) }
|  IDENTIFIER    optional_varlist(varlist)     {Ast.Open_prop (Ast.DEFIN($1),$2) }
| proposition AND proposition  { Ast.Coposition ($1,Ast.Conj,$3)  }
| proposition OR proposition  { Ast.Coposition ($1,Ast.Disjun,$3)  }


varlist:   
       | {[]}
       |  IDENTIFIER  {[$1]}
       |  IDENTIFIER COMMA varlist   {$1::$3}
       ;
        


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
  | IDENTIFIER   optional_varlist(varlist)    {Ast.Var ($1) }
  |  Num                {Ast.INT $1}
  |  LPAR expr RPAR    { $2 }
  | SLPAR IDENTIFIER IN IDENTIFIER ARROW  expr SRPAR {Ast.Func_def (Ast.Var($2),Ast.Var $4 ,$6) } ;

  
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
