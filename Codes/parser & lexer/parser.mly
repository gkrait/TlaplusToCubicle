 

 (* Since our parser handles a Specs written by a fragment of TLA++
    we start by defining the same tokens that already are in TLA++ Parser 
    restricting its rules *)




(* George: Still not a complete list *)
%token TOK_EOF
%token <string> TOK_IDENT
%token TOK_LPAR
%token TOK_RPAR
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_NOT
%token TOK_CONJ
%token TOK_DISJ
%token TOK_IMPL
%token TOK_EQUIV
%token TOK_COMMA
%token TOK_FORALL
%token TOK_EXISTS
%token TOK_ALWAYS
%token TOK_ALWAYSP
%token TOK_SOMETIME
%token TOK_SOMETIMEP
%token TOK_NEXT
%token TOK_UNTIL
%token TOK_TRUE
%token TOK_FALSE
%token TOK_UNLESS
%token TOK_UNDERSCOR 

%left TOK_UNTIL
%left TOK_UNLESS
%left TOK_EQUIV
%left TOK_DISJ
%left TOK_IMPL
%left TOK_CONJ
%left TOK_NOT Quantification
%left TOK_SOMETIME
%left TOK_ALWAYS
%left TOK_NEXT
%start start
%type <Fotypes.formula> start

%%    



start:  spec  TOK_EOF 
           {
               $1;
           }

spec:  init  TOK_CONJ  TOK_LBRACKET TOK_RBRACKET    next TOK_UNDERSCOR 
         {}










