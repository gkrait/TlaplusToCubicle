type binop = 
  | Add
  | Sub
  | Mul
  | Div



type exp = 
  | Func_def of exp * exp * exp 
  | Var of string
  | STRING of string
  | Func_img of exp * string list  
  | INT of string
  | Binop of exp * binop * exp
  | Func_exception of exp * exp * exp 
  | TRUE
  | FALSE




type ldef_sides=
| DEFIN of string 


type uni=
|  Col 
| Prim
| NEWL 




type coparism =
| EQ
| Greater
| Less
| Inclus



type logicalop =
| Conj 
| Disjun
| ASSIG 

type prop = 
| Equality of exp  * exp 
| Inequality of exp * coparism * exp
| Not_equal of exp * exp 
| Inclusion of exp * string list 
| Coposition of prop * logicalop * prop 
| UNCHAN of string list 
| Open_prop of string* string list 




type quanti=
| Exis 
| Univ

type pred = 
| Prop of prop
| Existence of quanti * string list  * exp  * pred
| Universal of quanti * string list  * exp  * pred
| Pred_Comp of pred * logicalop * pred 
| Open_pred of string * string list

type arrow=
| Arrow of  prop * exp   

type temp=
| Predec of pred
| Prime of exp * string list * exp 
| Func_except of exp * string list * exp 
| CASES  of exp *  string list * arrow list
| Mix of temp * logicalop * temp
| Negation of temp 
| Open_temp of string * string list
| Implication of temp * temp





 

type rdef_sides=
| Expr of exp 
| Stat of temp 


type definition =
| Statment of ldef_sides * string list * logicalop * rdef_sides * uni
| Value of ldef_sides *  string list *  logicalop * rdef_sides * uni



type tla_file_taile = 
| Definition of definition  
| MulDef  of definition list 


type declarat= 
| VARI of string list 
| CONS  of string list 

type tla_file = 
| File of declarat * declarat *  tla_file_taile  



type obj_info = 
| ElE of   ldef_sides  * string list * string *  rdef_sides * string
