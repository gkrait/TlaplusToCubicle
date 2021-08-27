type binop = 
  | Add
  | Sub
  | Mul
  | Div


type exp = 
  | Var of string
  | Binop of exp * binop * exp


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




type quanti=
| Exis 
| Univ

type pred = 
| Prop of prop
| Existence of quanti * exp * coparism * exp * uni * pred
| Universal of quanti * exp * coparism * exp * uni * pred



type temp=
| Predec of pred
| Prime of exp * uni * coparism * exp 
| Mix of temp * logicalop * temp

type assig_value=
| Expr of exp 
| Stat of temp 


type definition =
| Statment of exp * logicalop * assig_value * uni
| Value of exp * logicalop * assig_value * uni


type tla_fil = 
| Definition of definition  
| MulDef  of tla_fil * tla_fil




type obj_info = 
| ElE of   exp *  string *  assig_value *string

