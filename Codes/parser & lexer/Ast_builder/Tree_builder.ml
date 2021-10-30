 
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

 
let rec sublist l i j =
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j)


let rec parse_tla_taile fil = match fil with 
| Ast.MulDef (defns_list) -> begin  let n=(List.length defns_list) in 
         match  n  with 
      | 0 -> []
      | 1 ->   parse_def (List.nth defns_list 0)  ::[]
      | _ ->   let sub= (sublist defns_list 1 (n-1) )  in 
                  let img_sub=    parse_tla_taile  (Ast.MulDef (sub)) in 
                    let head = [parse_def (List.nth defns_list 0 )] in 
                        head @ img_sub;
                 end 


let rec parse_tla  (Ast.File(Ast.VARI (var), Ast.CONS(con) , fil  ))   =
let b= ("\narray ") ^(List.nth  var 0) ^("[proc] : int \n \n") in 
let a= (print_string   b)  in  
parse_tla_taile fil 




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



let  rec tla_to_cub_comp biop = match biop with 
 | Ast.EQ -> Cubicle_tree.EQ 
 | Ast.Greater -> Cubicle_tree.Greater 
 | Ast.Less -> Cubicle_tree.Less 
 | Ast.Inclus -> Cubicle_tree.Inclus 


(* Problem with  A[x]= 10  
it understands as "Var equal Int" However, it is supposed to understand it as "Func_img equal Int"    *)
let rec trans_equality  eq  = let Ast.Equality(l1 ,equa ,l2) =eq   in 
  match l1 with 
    
   | Ast.Func_img  ( exp1 , exp2)  -> begin  let Ast.Var(a)  = exp1 in     
        match l2 with 
                  | Ast.INT (num) -> Cubicle_tree.ELEstat("unprime  equality integer function image" ,
                                                     Cubicle_tree.Equality(Cubicle_tree.Func_img( Cubicle_tree.Var(a), Cubicle_tree.Var("z") ),
                                                      Cubicle_tree.INT(num) ) ) 
                   | Ast.Var v ->  
                   Cubicle_tree.ELEstat ("unprime  equality string function image", 
                         Cubicle_tree.Equality(Cubicle_tree.Var(a), Cubicle_tree.Var(v) ))  end

    | Ast.Var x1 ->  
      begin  
       match l2 with 
                 | Ast.Func_def(x,proc, value) ->  begin match value with
                                            | Ast.INT num ->    Cubicle_tree.ELEstat ("unprime function equality integer" ,
                                                    Cubicle_tree.Equality( Cubicle_tree.Func_img(Cubicle_tree.Var(x1) , Cubicle_tree.Var("z") )   , (* here assuming one variable case and Proc is the usual one *)

                                                      Cubicle_tree.INT(num) )) 
                                            | Ast.Var v -> Cubicle_tree.ELEstat ("unprime  equality string", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.Var(v) ) )   
                                            
                                            | _ -> Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("x"), Cubicle_tree.Var("x") ))
                                           end
                 | Ast.Var(v) -> Cubicle_tree.ELEstat ("unprime  equality var", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.Var(v) ) )
                 | Ast.INT num-> Cubicle_tree.ELEstat ("unprime  equality int", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.INT(num) ) ) 
                 | Ast.Binop (exp1 , bi , exp2)-> Cubicle_tree.ELEstat ("Uncovered unprime mix exp", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.Var ("z") ) )                                                                    
                                                   end

     



                                       
  | _ -> Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("x"), Cubicle_tree.Var("x") ))                                                                   



let rec change_log_type log= match log with 
| Ast.Conj -> Cubicle_tree.Conj
| Ast.Disjun -> Cubicle_tree.Disjun




let rec trans_prop prop = match prop with 
| Ast.Equality (exp1 , com , exp2) ->  (trans_equality (Ast.Equality (exp1 , com , exp2)))
| Ast.Inequality (exp1 , com , exp2) ->      let  Cubicle_tree.ELEstat ( str, Cubicle_tree.Equality(l1,l2)) = (trans_equality (Ast.Equality (exp1 , com , exp2))) in 
                                                       Cubicle_tree.ELEstat ("unprime  inequality",Cubicle_tree.Inequality(l1,tla_to_cub_comp com ,l2)  )
| Ast.Coposition  (prop1 , logicalop , prop2) ->  let  Cubicle_tree.ELEstat (str1,stat1) =  trans_prop  prop1  in 
                                                 let   Cubicle_tree.ELEstat (str2,stat2)=  trans_prop  prop2 in
                                                 let log = change_log_type logicalop in 
                                                  Cubicle_tree.ELEstat("proposition", Cubicle_tree.Coposition(stat1,log ,stat2) ) 
|  Ast.Open_prop (ldef_sides,  str) ->  Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("c"), Cubicle_tree.Var("x") ))




let  rec tla_to_cub_biop biop = match biop with 
 | Ast.Add -> Cubicle_tree.Add 
 | Ast.Sub -> Cubicle_tree.Sub 
 | Ast.Mul -> Cubicle_tree.Mul 
 | Ast.Div -> Cubicle_tree.Div 

 let  rec tla_to_cub_log biop = match biop with 
  | Ast.Conj -> Cubicle_tree.Conj 
 | Ast.Disjun -> Cubicle_tree.Disjun 



let rec tla_to_cub_exp(expr)= match expr with 
 | Ast.Var(v) -> Cubicle_tree.Var(v)
 | Ast.INT(n) -> Cubicle_tree.INT(n)
 | Ast.Binop(e1, bio , e2) -> Cubicle_tree.Binop(tla_to_cub_exp(e1), tla_to_cub_biop bio , tla_to_cub_exp(e2))
 | Ast.Func_img(e1,e2) -> Cubicle_tree.Func_img(tla_to_cub_exp e1,tla_to_cub_exp e2)

     

let rec trans_pred pred = match pred with 
   |  Ast.Prop prop ->  trans_prop prop
   | Ast.Universal (quanti , Ast.Var(v) , coparism , Ast.Var(z) , uni , pred1)  -> 
            match pred1 with 
                | Ast.Prop pro -> begin match  pro with 
                       |  Ast.Equality  (exp1 , coparism , exp2)  ->
                           let l1=tla_to_cub_exp exp1 in 
                            let l2=tla_to_cub_exp exp2 in 
                                  Cubicle_tree.ELEstat ("unprime  equality",
                                    Cubicle_tree.Equality(l1 , l2)  )

                       | Ast.Inequality (exp1 , coparism , exp2) -> 
                                  let l1=tla_to_cub_exp exp1 in 
                                  let l2=tla_to_cub_exp exp2 in 
                                  Cubicle_tree.ELEstat ("unprime  inequality",Cubicle_tree.Inequality(l1, tla_to_cub_comp coparism,l2)  )

                        | _ ->   Cubicle_tree.ELEstat(  "unprime function equality integer"  ,
                          Cubicle_tree.Equality( Cubicle_tree.Func_img(Cubicle_tree.Var("v") ,
                             Cubicle_tree.Var("z") )  ,
                              Cubicle_tree.Var("v") )  )       end 





         (* Cubicle_tree.ELEstat ( "To Check", trans_pred pred1)

       Cubicle_tree.Equality ( Cubicle_tree.Var("xnxnxnn"), 
          Cubicle_tree.Var(z) ) )  *)


let rec drop_str info= match info with 
 |Cubicle_tree.ElEassig(str, stat) -> Cubicle_tree.Primed(stat) 
 |Cubicle_tree.ELEstat(str, stat )  -> Cubicle_tree.Pred (Cubicle_tree.Prop  stat )
 | Cubicle_tree.ELEMix(str,stat) -> stat


let rec trans_temp obj =   match obj with 
| Ast.Predec   pred ->  trans_pred pred
(*| Ast.Prime  (exp1 , string_list , exp2)  ->  Cubicle_tree.ELEstat ("prime  equality",Cubicle_tree.Inequality(l1, l2)  ) *)
| Ast.Func_except (exp1,var_list,Ast.Func_exception (e1,e2,e3) ) -> Cubicle_tree.ElEassig ("primed equality except" ,  
                Cubicle_tree.Cases( tla_to_cub_exp exp1, tla_to_cub_exp e1, tla_to_cub_exp e2,
                tla_to_cub_exp e3)      ) (*George: Assuming that e1=exp1 *)
|Ast.Prime (exp1 , string_list , exp2 ) ->   Cubicle_tree.ElEassig ("primed equality" , 
      Cubicle_tree.Primed_assig(tla_to_cub_exp exp1 ,tla_to_cub_exp exp2 )) 
| Ast.Mix (temp1, logicalop , temp2) -> let trans1 = trans_temp temp1 in 
                                        let trans2 = trans_temp temp2 in
                                       Cubicle_tree.ELEMix( "mix" , 
                                        Cubicle_tree.Temp_Combination (drop_str trans1 , tla_to_cub_log logicalop ,drop_str trans2 )  )





let rec print_Cubobj  name cub_obj = match cub_obj with 
     | Cubicle_tree.ELEstat(str,stat ) -> begin  match str with 
           | "equality integer" -> Cub_print.print_prop  stat
           | _ -> Cub_print.print_prop  stat
         end 
      | Cubicle_tree.ElEassig (str, assig) -> Cub_print.print_primed  assig    
      | Cubicle_tree.ELEMix (str, stat ) -> Cub_print.print_temp name stat 
           
                 

let rec translate fil =
   let  obj_list=   parse_tla fil in 
   let  l= List.length obj_list in 
   begin
      for i= 0 to l-1 do 
        let  obji= List.nth  obj_list i in 
         let  Ast.ElE (DEFIN exp1 ,var_list, str1,  Ast.Stat (stat), str2 ) =obji in
         let   tra=  trans_temp stat in   
          let result=  print_Cubobj  exp1 tra in 
          match stat with 
            | Ast.Predec pred -> if exp1 ="Init" then  
                    print_string ("init (z) {"  ^  result ^ ("}\n"))
              else   print_string ( exp1 ^ " (z) {  "  ^  result ^ ("}\n")) 
            | Ast.Mix (temp1 , logicalop , temp2)  ->    
                          let Cubicle_tree.ELEMix(stri, temp_trans )  = tra in  
                                         print_string  (Cub_print.print_temp  exp1 temp_trans )    
      done; 
   end




(*              let oc = open_out "output.cub" in 
               Printf.fprintf oc "%s" (exp1 ^ "  == " ^ cub_string );  
               close_out oc;  *)




