type binop = 
  | Add
  | Sub
  | Mul
  | Div


type exp = 
  | Var of string
  | Func_img of exp * exp 
  | INT of string
  | Binop of exp * binop * exp
  | Open_exp of string * string list 
  | Func_def of exp * exp * exp 

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
| Equality of exp * coparism * exp 
| Inequality of exp * coparism * exp
| Coposition of prop * logicalop * prop 
| Open_prop of ldef_sides* string list 




type quanti=
| Exis 
| Univ

type pred = 
| Prop of prop
| Existence of quanti * exp * coparism * exp * uni * pred
| Universal of quanti * exp * coparism * exp * uni * pred
| Pred_Comp of pred * logicalop * pred 



type temp=
| Predec of pred
| Prime of exp * string list * exp 
| Mix of temp * logicalop * temp


type rdef_sides=
| Expr of exp 
| Stat of temp 


type definition =
| Statment of ldef_sides * string list * logicalop * rdef_sides * uni
| Value of ldef_sides *  string list *  logicalop * rdef_sides * uni



type tla_fil = 
| Definition of definition  
| MulDef  of tla_fil * tla_fil




type obj_info = 
| ElE of   ldef_sides  * string list * string *  rdef_sides * string



