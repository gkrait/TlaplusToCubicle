%{ %}

%token EOF
%token PLUS MINUS STAR SLASH EQUAL Larger Smaller OR AND  EXISTS FORALL
%token LPAR RPAR COLON IN PRIME SEMICOLON ASSIGN
%token <string> IDENTIFIER

%start <Ast.tla_fil> start
%%

/* Productions */
start : tla_file EOF       { $1 };

tla_file: 
| definition   {Ast.Definition ($1) }
| tla_file  tla_file {Ast.MulDef ($1, $2)}


definition:
| IDENTIFIER ASSIGN temporal_formula SEMICOLON {Ast.Statment (Ast.Var $1,Ast.ASSIG , Ast.Stat $3 , Ast.NEWL ) }
| IDENTIFIER ASSIGN expr SEMICOLON {Ast.Value (Ast.Var $1,Ast.ASSIG , Ast.Expr $3 , Ast.NEWL ) }


 temporal_formula:
| predicate { Ast.Predec $1}
| IDENTIFIER PRIME  EQUAL expr { Ast.Prime (Ast.Var $1, Ast.Prim , Ast.EQ , $4 ) }
|  temporal_formula OR temporal_formula  {Ast.Mix ($1,Ast.Disjun,$3) }
| temporal_formula AND temporal_formula  {Ast.Mix ($1,Ast.Conj,$3) }





 predicate:
| proposition {Ast.Prop $1 }
| EXISTS IDENTIFIER IN IDENTIFIER COLON  predicate   { Ast.Existence (Ast.Exis,Ast.Var $2, Ast.Inclus, Ast.Var $4, Ast.Col, $6 ) }
| FORALL IDENTIFIER IN IDENTIFIER COLON  predicate   { Ast.Universal (Ast.Univ,Ast.Var $2, Ast.Inclus, Ast.Var $4, Ast.Col, $6 ) }



proposition
: expr EQUAL expr      { Ast.Equality ($1,Ast.EQ,$3) }
| expr Larger expr     { Ast.Inequality ($1,Ast.Greater,$3) }
| expr Smaller expr    { Ast.Inequality ($1,Ast.Less,$3) }
| proposition AND proposition  { Ast.Coposition ($1,Ast.Conj,$3)  }
| proposition OR proposition  { Ast.Coposition ($1,Ast.Disjun,$3)  }



expr
  :  expr PLUS term    { Ast.Binop ($1,Ast.Add,$3) }
  |  expr MINUS term   { Ast.Binop ($1,Ast.Sub,$3) }
  |  term              { $1 };

term
  :  term STAR factor  { Ast.Binop($1,Ast.Mul,$3) }
  |  term SLASH factor { Ast.Binop($1,Ast.Div,$3) }
  |  factor            { $1 };

factor
  :  IDENTIFIER        { Ast.Var $1 }
  |  LPAR expr RPAR    { $2 };
