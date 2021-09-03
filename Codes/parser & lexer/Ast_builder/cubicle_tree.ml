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




  type obj_info = 
| ElE of   string  * exp * exp