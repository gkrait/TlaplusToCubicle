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



type coparism =
| EQ
| Greater
| Less
| Inclus


type logicalop =
| Conj 
| Disjun




type prop= 
| Equality of exp  * exp 
| Inequality of exp  * exp 
| Coposition of prop * logicalop * prop 


type pred =
| Prop of prop
| Existence of  exp * exp * pred
| Universal of  exp * exp * pred



type obj_info = 
| ElEassig of   string * exp *  prop
| ELEstat of string  *  prop











