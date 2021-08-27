let print_op op = match op with
  | Ast.Add -> print_string "+"
  | Ast.Sub -> print_string "-"
  | Ast.Mul -> print_string "*"
  | Ast.Div -> print_string "/"

let rec print_exp exp = match exp with
  | Ast.Var v -> 
    print_string v
  | Ast.Binop (exp0, op, exp1) ->
    begin
      print_string "(";
      print_exp exp0;
      print_op op;
      print_exp exp1;
      print_string ")";
    end
let rec print_compar compar = match compar with 
| Ast.EQ -> print_string "="
| Ast.Greater -> print_string ">"
| Ast.Less -> print_string "<"
| Ast.Inclus -> print_string "\\in"


let rec print_log oper= match oper with 
| Ast.Conj -> print_string "/\\"
| Ast.Disjun -> print_string "\\/"
| Ast.ASSIG -> print_string "==" 

let rec print_prop prop= match prop with 
  | Ast.Equality (exp1, oper, exp2) | Ast.Inequality (exp1, oper, exp2) -> 
         begin
      print_string "[";
      print_exp exp1;
      print_compar oper ;
      print_exp exp2;
      print_string "]";
         end
  | Ast.Coposition (prop1, oper, prop2) -> 
         begin
      print_newline () ;
      print_prop prop1;
      print_newline () ;
      print_log oper ;
      print_newline () ;
      print_prop prop2;
      print_newline () ;
         end         

let rec print_pred pred= match pred with 
  | Ast.Prop pr ->  print_prop pr
  | Ast.Existence (quanti , exp1 ,incl , exp2,col ,  pred0)  -> 
         begin
      print_string "\\E";
      print_exp exp1;
      print_string "\\in" ;
     print_exp exp2;
      print_newline () ;
      print_pred pred0 ;
         end
  | Ast.Universal (quanti , exp1 ,incl , exp2,col ,  pred0) -> 
         begin
      print_string "\\A";
      print_exp exp1;
      print_string "\\in" ;
     print_exp exp2;
      print_newline () ;
      print_pred pred0 ;
         end  

let rec print_temp temp= match temp with               
| Ast.Predec pred -> print_pred pred
| Ast.Prime (exp1, uni , coparism , exp2 ) ->
   begin
    print_exp exp1 ;
    print_string "'";
    print_compar coparism;
    print_exp exp2 ;
  end
 | Ast.Mix (temp1 , logicalop , temp2) ->
   begin 
     print_temp temp1 ;
     print_newline () ;
     print_log logicalop ;
     print_newline () ;
     print_temp temp2 ;
    end 



let rec parse_def def = match def with 
| Ast.Value (exp1 , logicalop , exp2 , uni) -> Ast.ElE ( exp1 ,  "value",  exp2, "not leaf" )  
| Ast.Statment (exp1 , logicalop , temp , uni) ->  Ast.ElE  (exp1 ,  "statment" , temp, "not leaf" )  






let rec parse_tla fil=match fil with   
| Ast.Definition def -> parse_def def:: []
| Ast.MulDef (tla_fil1 , tla_fil2) -> parse_tla tla_fil1 @   parse_tla tla_fil2

(*

let rec print_obj obj (ex1,str1,assig,str2) =
  begin 
  print_exp   ex1; 
  print_string  (" is assigned to   ") ;
end 

let rec print_Objs fil =
   
   let  obj_list=  parse_tla fil in 
   let  l= List.length obj_list in 
   begin
    print_int l;
    for i= 0 to l-1 do 
     let  obji= List.nth  obj_list i in 
     begin 
      print_obj obji;
      print_newline () ;
    end 
      done; 
  end

*)


