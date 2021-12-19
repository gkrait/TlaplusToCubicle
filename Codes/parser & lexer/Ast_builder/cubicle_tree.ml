type binop = 
  | Add
  | Sub
  | Mul
  | Div


type exp = 
  | Var of string
  | Func_img of exp * string list  
  | INT of string
  | Binop of exp * binop * exp
  | STRING of string  
  | TRUE
  | FALSE
  | Func_def of exp * exp * exp



type coparism =
| EQ
| Greater
| Less
| Inclus


type logicalop =
| Conj 
| Disjun
| ASSIG 



type prop= 
| Equality of exp  * exp 
| Inequality of exp  * coparism * exp 
| Not_equal of exp * exp 
| Coposition of prop * logicalop * prop 
| Declaration of exp * string list * string 
| UNCHAN 
| Open_prop of string * string list 


type pred =
| Prop of prop
| Existence of  exp * exp * pred
| Universal of  exp * exp * pred
| Pred_Comp of pred * logicalop * pred 


type primed_equality=
| Primed_assig of exp * exp  
| Cases of exp * exp * exp * exp 

type arrow = 
| Arrow  of prop * exp 

type temp = 
| Pred of pred 
| Primed of primed_equality 
| Open_temp of string * string list
| Temp_Combination of temp * logicalop * temp  
| Negation of temp 
| CASES of exp *  string list * arrow list


type obj_info = 
| ElEassig of   string * primed_equality 
| ELEstat of string  *  prop
| ELEMix of string  *  temp 











