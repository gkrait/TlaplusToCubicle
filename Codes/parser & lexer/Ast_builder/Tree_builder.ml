


let print_op op = match op with
  | Ast.Add -> print_string "+"
  | Ast.Sub -> print_string "-"
  | Ast.Mul -> print_string "*"
  | Ast.Div -> print_string "/"


let rec print_Strlist = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_Strlist l ;;

let rec print_exp exp = match exp with
  | Ast.Var v -> 
    print_string v
  | Ast.INT  (num)-> print_string num  
  | Ast.Open_exp (v,var_list) -> begin  print_string v; print_string "(" ; print_Strlist var_list; print_string ")"; end
  | Ast.Func_img (var1,var2) -> 
    begin 
      print_exp var1 ;
      print_string "[" ;
      print_exp var2 ;
      print_string "]"   
    end  

  | Ast.Binop (exp0, op, exp1) ->
    begin
      print_string "(";
      print_exp exp0;
      print_op op;
      print_exp exp1;
      print_string ")";
    end
  | Ast.Func_def (var1,var2,exp) ->
    begin
      print_string "[ ";
      print_exp var1; print_string " \\in ";
      print_exp var2; print_string " |-> ";
      print_exp exp; print_string " ]";
    end


let rec print_compar compar = match compar with 
| Ast.EQ -> print_string " = "
| Ast.Greater -> print_string " > "
| Ast.Less -> print_string " < "
| Ast.Inclus -> print_string " \\in "


let rec print_log oper= match oper with 
| Ast.Conj -> print_string " /\\ "
| Ast.Disjun -> print_string " \\/ "
| Ast.ASSIG -> print_string "== " 



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
      print_prop prop1;
      print_log oper ;
      print_prop prop2;
         end         
  | Ast.Open_prop (Ast.DEFIN(v),var_list) -> begin print_string v; print_string "(" ; print_Strlist var_list; print_string ")"; end  



let rec print_pred pred= match pred with 
  | Ast.Prop pr ->  print_prop pr
  | Ast.Existence (quanti , exp1 ,incl , exp2,col ,  pred0)  -> 
         begin
      print_string "  \\E   ";
      print_exp exp1;
      print_string "  \\in  " ;
     print_exp exp2;
      print_pred pred0 ;
         end
  | Ast.Universal (quanti , exp1 ,incl , exp2,col ,  pred0) -> 
         begin
      print_string " \\A  ";
      print_exp exp1;
      print_string "  \\in  " ;
     print_exp exp2;
      print_pred pred0 ;
         end  
| Ast.Pred_Comp (pred1,con,pred2) ->  print_pred pred1 ; print_pred pred2 




 let rec print_temp temp= match temp with               
| Ast.Predec pred -> print_pred pred
| Ast.Prime (exp1 ,var_list ,  exp2 ) ->
   begin
    print_exp exp1 ;
    print_string "'";
     print_string "(" ; print_Strlist var_list; print_string ")";
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
| Ast.Value (exp1, var_list,logicalop , exp2 , uni) -> Ast.ElE ( exp1 ,var_list, "value",  exp2, "not leaf" )  
| Ast.Statment (exp1 ,var_list, logicalop , temp , uni) ->  Ast.ElE (exp1  , var_list, "statment" , temp, "not leaf" )  

 

let rec parse_tla fil=match fil with   
| Ast.Definition def -> parse_def def:: []
| Ast.MulDef (tla_fil1 , tla_fil2) -> parse_tla tla_fil1 @   parse_tla tla_fil2



let rec print_obj  (Ast.ElE (Ast.DEFIN(v),var_list,str1,assig,str2) )=
 begin 
   print_string  v; print_string "(" ; print_Strlist var_list; print_string ")";
   print_string  (" is assigned to   ") ;
   match assig with 
    | Ast.Expr (expr2) -> print_exp   expr2;
    | Ast.Stat (temp) -> print_temp temp ;
 end 




let rec print_Objs fil =
   let  obj_list=  parse_tla fil in 
   let  l= List.length obj_list in 
   begin
    for i= 0 to l-1 do 
     let  obji= List.nth  obj_list i in 
     begin 
      print_obj obji;
      print_newline () ;
    end 
      done; 
  end





(*             translating     *)


let rec trans_equality  eq  = let Ast.Equality(l1 ,equa ,l2) =eq   in 
  match l1 with 
  | Ast.Var x1 ->
    begin  
      match l2 with 
                 | Ast.Func_def(x,proc, value) ->  begin match value with
                                            | Ast.INT (num) ->    Cubicle_tree.ELEstat ("unprime  equality integer" ,
                                                     Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.INT(num) )) 
                                            | Ast.Var v -> Cubicle_tree.ELEstat ("unprime  equality string", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.Var(v) ) )  end 
                                            | _ -> Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("x"), Cubicle_tree.Var("x") ))
    end 
  | Ast.Func_img  ( exp1 , exp2)  -> begin  let Ast.Var(a)  = exp1 in     
        match l2 with 
                  | Ast.INT (num) -> Cubicle_tree.ELEstat("unprime  equality integer function image" ,
                                                     Cubicle_tree.Equality(Cubicle_tree.Var(a), Cubicle_tree.INT(num) ) ) 
                   | Ast.Var v ->  
                   Cubicle_tree.ELEstat ("unprime  equality string function image", 
                         Cubicle_tree.Equality(Cubicle_tree.Var(a), Cubicle_tree.Var(v) ))  end


                                       
  | _ -> Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("x"), Cubicle_tree.Var("x") ))                                                                   



let rec change_log_type log= match log with 
| Ast.Conj -> Cubicle_tree.Conj
| Ast.Disjun -> Cubicle_tree.Disjun




let rec trans_prop prop = match prop with 
| Ast.Equality (exp1 , com , exp2) ->  (trans_equality (Ast.Equality (exp1 , com , exp2)))
| Ast.Inequality (exp1 , com , exp2) ->      let  Cubicle_tree.ELEstat ( str, Cubicle_tree.Equality(l1,l2)) = (trans_equality (Ast.Equality (exp1 , com , exp2))) in 
                                                       Cubicle_tree.ELEstat ("unprime  inequality",Cubicle_tree.Inequality(l1, l2)  )
| Ast.Coposition  (prop1 , logicalop , prop2) ->  let  Cubicle_tree.ELEstat (str1,stat1) =  trans_prop  prop1  in 
                                                 let   Cubicle_tree.ELEstat (str2,stat2)=  trans_prop  prop2 in
                                                 let log = change_log_type logicalop in 
                                                  Cubicle_tree.ELEstat("proposition", Cubicle_tree.Coposition(stat1,log ,stat2) ) 
                                                
| _ ->  Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("x"), Cubicle_tree.Var("x") ))



let rec trans_obj obj = let Ast.ElE ( exp1 ,var_list, str1,  Ast.Stat (prop1), str2 ) =obj in 
   match prop1 with 
| Ast.Predec(Ast.Prop prop2 ) ->  trans_prop prop2
| _ ->  (Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("x"), Cubicle_tree.Var("x") )))


(*
let rec trans_pred pred = match pred with 
   |  Ast.Prop prop ->  
   | Ast.Universal (quanti , exp1 , coparism , exp2 , uni , pred1) *)






 (*

let rec trans_temp tf =  match tf with 
  | Ast.Predec pred -> trans_pred pred
  | Ast.Prime  (exp1 , varlist , exp2) -> 
  | Ast.Mix  (temp1 , logicalop , temp2) ->  

*)

     


let rec print_Cubobj   cub_obj = match cub_obj with 
     | Cubicle_tree.ELEstat(str,stat ) ->   "Init (" ^ "z"  ^ ") = { " ^  str ^ "(" ^ "z" ^ ")=" ^ str  ^ " }" 
     | _ ->  "Init ss" 


let rec translate fil =
   let  obj_list=   parse_tla fil in 
   let  l= List.length obj_list in 

   begin
      for i= 0 to l-1 do 
        let  obji= List.nth  obj_list i in 
          let tra=  trans_obj obji in 
          let cub_string = print_Cubobj(tra) in  
            let oc = open_out "output.cub" in (* create or truncate file, return channel *)
               Printf.fprintf oc "%s\n" cub_string; (* write something *)   
               close_out oc;  
      done; 
   end

