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


type primed_equality=
| Primed_assig of exp * exp  
| Cases of exp * exp * exp * exp 


type temp = 
| Pred of pred 
| Primed of primed_equality 
| Combination of temp * logicalop * temp  



type obj_info = 
| ElEassig of   string * primed_equality 
| ELEstat of string  *  prop
| ELEMix of string  *  temp 











