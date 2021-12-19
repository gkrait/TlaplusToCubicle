type cub_declaration = {type_dec : string list  ; array_dec : string list } 


let check_str s = 
  try int_of_string s |> ignore; "int"
  with Failure _ -> if (s ="FALSE" || s= "TRUE") then "bool" else "str"

let print_op op = match op with
  | Ast.Add -> print_string "+"
  | Ast.Sub -> print_string "-"
  | Ast.Mul -> print_string "*"
  | Ast.Div -> print_string "/"

let rec tla_to_cub_biop biop = match biop with 
 | Ast.Add -> Cubicle_tree.Add 
 | Ast.Sub -> Cubicle_tree.Sub 
 | Ast.Mul -> Cubicle_tree.Mul 
 | Ast.Div -> Cubicle_tree.Div 

let  rec tla_to_cub_comp biop = match biop with 
 | Ast.EQ -> Cubicle_tree.EQ 
 | Ast.Greater -> Cubicle_tree.Greater 
 | Ast.Less -> Cubicle_tree.Less 
 | Ast.Inclus -> Cubicle_tree.Inclus 

let rec tla_to_cub_exp(expr)= match expr with 
 | Ast.STRING(v) -> Cubicle_tree.STRING(v)
 | Ast.INT(n) -> Cubicle_tree.INT(n)
 | Ast.Func_img(e1,vars) -> Cubicle_tree.Func_img(tla_to_cub_exp e1,vars )
 | Ast.Func_def(e1,e2,e3) -> Func_def(tla_to_cub_exp e1, tla_to_cub_exp e2, tla_to_cub_exp e3)
 | Ast.Func_exception(e1,e2,e3) ->  tla_to_cub_exp e3 
 | Ast.TRUE -> Cubicle_tree.TRUE
 | Ast.FALSE -> Cubicle_tree.FALSE
  | Ast.Var(v) -> Cubicle_tree.Var(v)
 | Ast.Binop(e1, bio , e2) -> Cubicle_tree.Binop(tla_to_cub_exp(e1), tla_to_cub_biop bio , tla_to_cub_exp(e2))
 


let rec print_Strlist = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_Strlist l ;;

let rec print_exp exp = match exp with
  | Ast.Var v -> 
    print_string v
  | Ast.INT  (num)-> print_string num  
  | Ast.STRING(str) -> print_string ("\"" ^ str  ^ "\"")
  | Ast.Func_img (var1,var2) -> 
    begin 
      print_exp var1 ;
      print_string "[" ;
     
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
| Ast.Conj ->  " /\\ "
| Ast.Disjun ->  " \\/ "
| Ast.ASSIG ->  "== " 


(*

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

*)
let rec parse_def def = match def with 
| Ast.Value (exp1, var_list,logicalop , exp2 , uni) -> Ast.ElE ( exp1 ,var_list, "value",  exp2, "not leaf" )  
| Ast.Statment (exp1 ,var_list, logicalop , temp , uni) ->  Ast.ElE (exp1  , var_list, "statment" , temp, "not leaf" )  

 
let rec sublist l i j =
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j)


let rec parse_tla (Ast.File(Ast.VARI (var), Ast.CONS(con) , fil  )) = 
   match fil with 
  | Ast.MulDef (defns_list) -> begin  let n=(List.length defns_list) in 
         match  n  with 
      | 0 -> []
      | 1 ->   parse_def (List.nth defns_list 0)  ::[]
      | _ ->   let sub= (sublist defns_list 1 (n-1) )  in 
                  let img_sub=    parse_tla  (Ast.File(Ast.VARI (var),
                                Ast.CONS(con) , Ast.MulDef (sub)  )) in 
                    let head = [parse_def (List.nth defns_list 0 )] in 
                        head @ img_sub;
                 end 

(*
let rec parse_tla  (Ast.File(Ast.VARI (var), Ast.CONS(con) , fil  ))   =
let b= ("\narray ") ^(List.nth  var 0) ^("[proc] : int  \n") in 
let a= (print_string   b)  in  
parse_tla_taile fil 
*)


(*
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
    end 
      done; 
  end

*)

(*             translating     *)






let rec trans_equality  eq  = let Ast.Equality(l1 ,equa ,l2) =eq   in 
  match l1 with 
    | Ast.Func_img  ( Ast.Var x1 , vars)  ->  
     begin  
       match l2 with 
                 | Ast.Func_def(x,proc, value) ->  let xx= tla_to_cub_exp x and procp= tla_to_cub_exp proc
                 and value_tr= tla_to_cub_exp value in 
                  Cubicle_tree.ELEstat("", 
                  Cubicle_tree.Equality ( tla_to_cub_exp l1, Cubicle_tree.Func_def(xx,procp, value_tr)  ) )
  
                 (* begin match value with
                                            | Ast.INT num ->    Cubicle_tree.ELEstat ("unprime function equality integer" ,
                                                    Cubicle_tree.Equality(Cubicle_tree.Var(x1)    , (* here assuming one variable case and Proc is the usual one *)
                                                      Cubicle_tree.INT(num) )) 
                                            | Ast.Var v -> Cubicle_tree.ELEstat ("unprime  equality var", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.Var(v) ) )   
                                            
                                            | Ast.STRING s -> Cubicle_tree.ELEstat ("unprime  equality string", 
                                                Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.STRING(s) ) )
                                            | Ast.TRUE ->  Cubicle_tree.ELEstat ("unprime  equality bool", 
                                                Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.TRUE ) )   
                                            | Ast.FALSE ->  Cubicle_tree.ELEstat ("unprime  equality bool", 
                                                Cubicle_tree.Equality(Cubicle_tree.Var(x1), Cubicle_tree.FALSE ) )                                                      
                                           end  *)
                 | Ast.Var(v) -> Cubicle_tree.ELEstat ("unprime  equality var", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.Var(v) ) )
                 | Ast.INT num-> Cubicle_tree.ELEstat ("unprime  equality int", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.INT(num) ) ) 
                 | Ast.Binop (exp1 , bi , exp2)-> Cubicle_tree.ELEstat ("Uncovered unprime mix exp", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.Var ("z") ) )                                                                    
                                                   
                 | Ast.STRING(s) ->   Cubicle_tree.ELEstat (" unprime  equality string", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1 , Cubicle_tree.STRING (s) ) ) 
                 | Ast.TRUE ->   Cubicle_tree.ELEstat (" unprime  equality bool", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.TRUE  ) )                                                             
                 | Ast.FALSE ->   Cubicle_tree.ELEstat (" unprime  equality bool", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.FALSE ) )                           
                 | Ast.Func_img(e1,e2) -> let e1_cub= tla_to_cub_exp e1 in 
                    Cubicle_tree.ELEstat (" unprime  equality var", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.Func_img(e1_cub,e2) ) )                            
                                             end 



                                       
  | _ -> Cubicle_tree.ELEstat( "uncovered", Cubicle_tree.Equality(Cubicle_tree.Var("zz"), Cubicle_tree.Var("x") ))                                                                   



let rec change_log_type log= match log with 
 | Ast.Conj -> Cubicle_tree.Conj
 | Ast.Disjun -> Cubicle_tree.Disjun



let rec trans_prop prop = match prop with 
 | Ast.Equality (exp1 , com , exp2) ->  (trans_equality (Ast.Equality (exp1 , com , exp2))) 
 | Ast.Inequality (exp1 , com , exp2) ->      let  Cubicle_tree.ELEstat ( str, Cubicle_tree.Equality(l1,l2)) = (trans_equality (Ast.Equality (exp1 , com , exp2))) in 
                                                    Cubicle_tree.ELEstat ("unprime  inequality", 
                                                    Cubicle_tree.Inequality(l1,tla_to_cub_comp com ,l2)  )
 |  Ast.Open_prop (v,var_list) ->  
                Cubicle_tree.ELEstat("open prop", Cubicle_tree.Open_prop(v,var_list))
 | Ast.Not_equal(exp1,exp2) -> 
    Cubicle_tree.ELEstat ("not equality", 
    Cubicle_tree.Not_equal(tla_to_cub_exp exp1 , tla_to_cub_exp exp2 ))
 | Ast.Inclusion(exp , string_list ) -> 
            let data_type= check_str (List.nth  string_list 0) in 
            if  data_type ="int" then  
                    Cubicle_tree.ELEstat( "declaration",
                          Cubicle_tree.Declaration(tla_to_cub_exp exp, string_list, "int" )  )  
            else if data_type ="bool" then 
                Cubicle_tree.ELEstat( "declaration", 
                          Cubicle_tree.Declaration(tla_to_cub_exp exp, string_list, "bool" ) )
            else   
                  Cubicle_tree.ELEstat( "declaration", 
                          Cubicle_tree.Declaration(tla_to_cub_exp exp, string_list, "str" ) )
                                
 | Ast.UNCHAN (str) ->  Cubicle_tree.ELEstat( "proposition", 
                          Cubicle_tree.UNCHAN)

 | Ast.Coposition  (prop1 , logicalop , prop2) ->  let  Cubicle_tree.ELEstat (str1,stat1) =  trans_prop  prop1  in 
                                                 let   Cubicle_tree.ELEstat (str2,stat2)=  trans_prop  prop2 in
                                                 let log = change_log_type logicalop in 
                                                  Cubicle_tree.ELEstat("proposition", Cubicle_tree.Coposition(stat1,log ,stat2) ) 

 |Ast.Open_prop(v, var_list) -> 
            Cubicle_tree.ELEstat("open prop", Cubicle_tree.Open_prop(v,var_list)  )
 

 let  rec tla_to_cub_log biop = match biop with 
  | Ast.Conj -> Cubicle_tree.Conj 
 | Ast.Disjun -> Cubicle_tree.Disjun 

let rec trans_pred pred =
 match pred with 
   |  Ast.Prop prop ->  trans_prop prop
   | Ast.Universal (quanti , var_list , coparism , Ast.Var(z) , uni , pred1)  -> begin
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
                                Cubicle_tree.ELEstat ("unprime  inequality",
                                    Cubicle_tree.Inequality(l1, tla_to_cub_comp coparism,l2)  )

                        |  Ast.Not_equal  (exp1  , exp2)  ->
                           let l1=tla_to_cub_exp exp1 in 
                            let l2=tla_to_cub_exp exp2 in 
                            Cubicle_tree.ELEstat ("unprime  not equal",
                                    Cubicle_tree.Not_equal(l1 , l2 )  )
                        | Ast.Coposition(pr1,log,pr2) ->
                            let  Cubicle_tree.ELEstat(strq,tra_pr1)=  trans_prop pr1 in 
                            let  Cubicle_tree.ELEstat(str2 ,tra_pr2)=  trans_prop pr2 in    
                            Cubicle_tree.ELEstat(  "unprime composition"  ,
                            Cubicle_tree.Coposition(tra_pr1, tla_to_cub_log log ,tra_pr2) )
                        | _ ->   Cubicle_tree.ELEstat(  "UNCOVERED"  ,
                          Cubicle_tree.Equality( Cubicle_tree.Func_img(Cubicle_tree.Var("v") ,
                             ["z"] )  ,
                              Cubicle_tree.Var("v") )  )       end 

                 | Ast.Pred_Comp(pr1,log,pr2) ->
                        let  Cubicle_tree.ELEstat(strq,tra_pr1)=  trans_pred pr1 in 
                        let  Cubicle_tree.ELEstat(str2 ,tra_pr2)=  trans_pred pr2 in    
                            Cubicle_tree.ELEstat(  "unprime composition"  ,
                          Cubicle_tree.Coposition(tra_pr1, tla_to_cub_log log,tra_pr2) ) (* George check here *)  end
   | Ast.Pred_Comp(pred1 , logicalop , pred2) -> 
                 let  Cubicle_tree.ELEstat(str1  ,  prop1) = trans_pred  pred1 in 
                 let  Cubicle_tree.ELEstat(str2  ,  prop2) = trans_pred  pred2 in 
                 let logi=tla_to_cub_log logicalop in 
                Cubicle_tree.ELEstat ("Mix", 
                Cubicle_tree.Coposition(prop1, logi ,prop2))
   | Ast.Existence (quanti , var_list , coparism , Ast.Var(z) , uni , pred1) -> 
           trans_pred  pred1
        (*Cubicle_tree.ELEstat ("Uncovered", 
                Cubicle_tree.Equality(Cubicle_tree.Var("x") , Cubicle_tree.Var("x"))) *)
   | Ast.Open_pred(stat_name, var_list) ->  Cubicle_tree.ELEstat ("open pred", 
                Cubicle_tree.Open_prop(stat_name, var_list))           
 

         (* Cubicle_tree.ELEstat ( "To Check", trans_pred pred1)

       Cubicle_tree.Equality ( Cubicle_tree.Var("xnxnxnn"), 
          Cubicle_tree.Var(z) ) )  *)


let rec drop_str info= match info with 
 |Cubicle_tree.ElEassig(str, stat) -> Cubicle_tree.Primed(stat) 
 |Cubicle_tree.ELEstat(str, stat )  -> Cubicle_tree.Pred (Cubicle_tree.Prop  stat )
 | Cubicle_tree.ELEMix(str,stat) -> stat

let rec tla_to_cub_arrow (arrows)= match arrows with
     | [arrow] ->  let  Ast.Arrow(prop,exp) = arrow in 
            let exp_cub= tla_to_cub_exp exp 
            and info_prop_cub = trans_pred  (Ast.Prop prop) in 
            let Cubicle_tree.ELEstat(str,prop_cub)= info_prop_cub in 
            [Cubicle_tree.Arrow(prop_cub,exp_cub)]
    | h::t -> tla_to_cub_arrow ([h]) @ tla_to_cub_arrow (t)        


let rec trans_temp obj =   match obj with 
 | Ast.Predec pred  ->  trans_pred pred
 | Ast.Func_except (exp1,var_list,Ast.Func_exception (e1,e2,e3) ) -> 
        Cubicle_tree.ElEassig ("primed equality except" ,  
                Cubicle_tree.Cases( tla_to_cub_exp exp1, tla_to_cub_exp e1, tla_to_cub_exp e2,
                tla_to_cub_exp e3)      ) (*George: Assuming that e1=exp1 *)
 |Ast.Prime (exp1 , string_list , exp2 ) ->   Cubicle_tree.ElEassig ("primed equality" , 
      Cubicle_tree.Primed_assig(tla_to_cub_exp exp1 ,tla_to_cub_exp exp2 )) 
 | Ast.Mix (temp1, logicalop , temp2) -> let trans1 = trans_temp temp1 in 
                                        let trans2 = trans_temp temp2 in
                                       Cubicle_tree.ELEMix( "mix" , 
                                        Cubicle_tree.Temp_Combination (drop_str trans1 , tla_to_cub_log logicalop ,drop_str trans2 )  )
 | Ast.Open_temp (stat_name,var_list) -> 
        Cubicle_tree.ELEMix("open temp",
          Cubicle_tree.Open_temp(stat_name,var_list)  )
 | Ast.Negation temp ->  let Cubicle_tree.ELEMix(str, trans) = trans_temp temp in  
 Cubicle_tree.ELEMix("negation",
          Cubicle_tree.Negation(trans)  )  
 | Ast.CASES(exp , vars , arrows ) ->  let cub_arrows= tla_to_cub_arrow arrows in 
       Cubicle_tree.ELEMix("Cases",
        Cubicle_tree.CASES(Cubicle_tree.Func_img((tla_to_cub_exp exp) , vars), vars , cub_arrows )  )             

let rec print_Cubobj  name cub_obj defs_dic = match cub_obj with 
    | Cubicle_tree.ELEstat(str,stat ) -> begin  match str with 
           | "equality integer" -> [( (Cub_print.fix_print_prop_output(Cub_print.print_prop  stat defs_dic)),"")]
           | _ -> [(Cub_print.fix_print_prop_output (Cub_print.print_prop  stat defs_dic) ,"")]
         end 
    | Cubicle_tree.ElEassig (str, assig) -> [("", Cub_print.print_primed  assig )]   
    | Cubicle_tree.ELEMix (str, stat ) ->  ((Cub_print.print_temp name stat defs_dic)  )  

           
  
let rec insert_at_end l i =
  match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)                 

  


let rec type_Ok  def= match def with 
  |   Ast.Predec( Ast.Prop ( prop ) ) -> begin  
    let tran = trans_prop prop in  
       let  (Cubicle_tree.ELEstat( str1, cub_prop)) =tran in  
          match cub_prop with  
            |  Cubicle_tree.Declaration ( exp, string_list, str2 ) ->  begin 
                match str2 with  
                   | "str"  ->  { type_dec = string_list ; array_dec= 
                          [("array ")  ^ (Cub_print.print_exp exp) ^ ("[proc] : string_type \n")] }
                   | "int" -> { type_dec = [] ; array_dec = 
                       [("array ")  ^ (Cub_print.print_exp exp) ^ ("[proc] : int \n")] } 
                   | "bool" ->  { type_dec = [] ; array_dec = 
                       [("array ")  ^ (Cub_print.print_exp exp) ^ ("[proc] : bool \n")] }          
                                                                       end 
            |  Cubicle_tree.Coposition(prop1, logicalop , prop2  )  -> 
             let Ast.Coposition(prop1, logicalop , prop2  ) = prop in 
                let res1 = type_Ok  (Ast.Predec (Ast.Prop (prop1))) in 
                let res2 =  type_Ok (Ast.Predec (Ast.Prop (prop2))) in
                 { type_dec= res1.type_dec @ res2.type_dec   ;  array_dec = res1.array_dec @res2.array_dec  }  end 
 | Ast.Mix(temp1,log,temp2)  -> 
        let res1 = type_Ok  temp1 in 
        let res2 = type_Ok  temp2 in 
  { type_dec= res1.type_dec @ res2.type_dec   ;  array_dec = res1.array_dec @res2.array_dec  }   



let rec non_trivial_con(str1,str2, sep)= 
    if str1 = "" && str2 = "" then ""
    else if  str1 = "" && str2 != "" then str2
    else if str1 != "" && str2 = "" then str1
    else let lpar= if (String.contains_from str1 0 ':') && (String.contains_from str1 0 '=')  then "" else "(" 
            and  rpar= if (String.contains_from str1 0 ':') && (String.contains_from str1 0 '=') then "" else ")" in 
         let sep_general= if  lpar = "(" then  sep else "" in  
    lpar ^ (str1) ^ rpar ^ (sep_general) ^ lpar ^ (str2) ^ rpar



let rec print_init init_stat defs_dic= match init_stat with 
    |   Ast.Predec( init_pred) -> let  stat_cub= trans_pred init_pred in 
        let  Cubicle_tree.ELEstat (str, prop_cub ) = stat_cub in 
        let  (stat_str,e2) = (( (Cub_print.print_prop prop_cub defs_dic) )) in
          stat_str ^e2
    | Ast.Mix(tem1,logic , tem2) -> 
         ("(") ^ ((print_init tem1 defs_dic) ) ^ (")") 
        ^ ( Cub_print.print_log (change_log_type logic) )
        ^ ("(") ^ (print_init tem2 defs_dic)  ^ (")")
    | Ast.Negation pred -> ("not ") ^("(") ^(print_init pred defs_dic) ^(")")
    | Ast.Open_temp (prop_name,  var_list) -> 
        let l= List.length defs_dic 
        and substitution = ref ("","") in
        for i= 0 to l-1 do 
            let (def_name, (def_stat, e))=List.nth defs_dic i in 
            if prop_name = def_name then  substitution := (def_stat, e)
            else substitution := !substitution;
            !substitution;
        done;
        let (e1,e2) = !substitution in 
        e1^e2


let rec remove_not_eqal safty_stat defs_dic = match safty_stat with 
    | Ast.Predec( Ast.Prop (Ast.Not_equal(Ast.Func_img(exp1,[]) ,Ast.Func_img(exp2,[]))) ) -> ""   
    | Ast.Mix(tem1,log, tem2) -> let stat1= remove_not_eqal tem1 defs_dic 
        and  stat2= remove_not_eqal tem2 defs_dic in 
        let log_str = match log with 
                |Ast.Conj -> " && " 
                |Ast.Disjun -> " || "  in 
        non_trivial_con(stat1,stat2,log_str)
    | _ ->   print_init safty_stat defs_dic                       


let rec print_intermid  intermid_stat defs_dic = match intermid_stat with 
        | Ast.Predec (pred) -> ((print_init intermid_stat defs_dic)  ,"") 
        | (Ast.Prime(exp1 , var_last , exp2)) | (Ast.Func_except(exp1 , var_last , exp2)) ->
            let stat_trans = trans_temp intermid_stat in
            let Cubicle_tree.ElEassig(info_str, trans_info)= stat_trans in
            let trans_str= Cub_print.print_primed trans_info in 
            ("",trans_str)
            (*let e1= tla_to_cub_exp exp1 
            and e2= tla_to_cub_exp exp2  in 
            ( "" , (Cub_print.print_exp e1) ^ (":=") ^ (Cub_print.print_exp e2) ) *)
        | Ast.Negation pred -> let (e1,e2) = print_intermid  pred  defs_dic in 
          (("~") ^ ("(") ^ (e1) ^ (")")  , 
                                ("~") ^ ("(") ^ (e2) ^ (")") ) (* George: I need to recheck here *)
        | Ast.Open_temp(name ,var_list) -> 
            let l= List.length defs_dic 
            and substitution = ref ("","") in
            for i= 0 to l-1 do 
                let (def_name, (pred_part,prim_part))=List.nth defs_dic i in 

                if name = def_name then   substitution := (pred_part,prim_part) 
                else substitution := !substitution; 
                !substitution;
                done;
                !substitution
        
        (*| Ast.Func_except(exp1 , var_last , exp2) ->
            ("", ( Cub_print.print_exp (tla_to_cub_exp exp1) ) ^ ("[z]:= ") ^ 
                ( Cub_print.print_exp (tla_to_cub_exp exp2) ) )      *) 
        | Ast.Mix(temp1,log, temp2) -> 
            let log_str = match log with 
                |Ast.Conj -> " && " 
                |Ast.Disjun -> " || " 
            in     
            let (pred1,prim1) =  print_intermid  temp1 defs_dic  
            and (pred2,prim2) =  print_intermid  temp2 defs_dic in 
            (non_trivial_con(pred1, pred2, log_str) ,
                non_trivial_con(prim1,prim2, log_str) )  
        | Ast.CASES (exp , vars , cub_arrows ) -> 
            let  info_cub_stat=  trans_temp intermid_stat in  
            let  Cubicle_tree.ELEMix(str,cub_stat)=info_cub_stat in 
             let [(e1,e2)]= (Cub_print.print_temp "" cub_stat defs_dic) in 
             (e1,e2)




let rec print_Next next_stat defs_dic name = let  tra=  trans_temp next_stat in 
    let transitions_str =  ref "" in 
    let stats_list= print_Cubobj  name tra defs_dic in 
    let l= List.length  stats_list in 
    for i =0 to l-1 do
    let (e1,e2) = List.nth stats_list i  in 
    if (e1,e2) = ("", "") then transitions_str := !transitions_str ^  ""
    else 
        transitions_str := !transitions_str ^ ("\ntransition ") ^ 
            (name) ^ ("_") ^(string_of_int (i+1)) ^ (" (z) \n") ^ ("requires {\n") ^
            (e1) ^("}\n") ^ ("{\n") ^(e2) ^("}\n")
    done;
    !transitions_str   

let rec translate ?ok:(typeOk_stat_name= "TypeOk")
                  ?saf:(safety_stat_name="Safety") 
                  ?ini:(init_stat_name= "Init")   
                  ?nex:(next_stat_name= "Next") 
                  ?spc:(spec_stat_name= "Spec")
                  fil  =
    let (Ast.File(Ast.VARI (var), Ast.CONS(con) , defs  )) = fil in 
    let  obj_list=   parse_tla fil in 
    let  l= List.length obj_list in 
    let defs_dic = ref [("",("",""))] in    
      begin
      let final_result =ref "" in
      for i= 0 to l-1 do 
        let  obji= List.nth  obj_list i in 
        let  Ast.ElE (DEFIN name ,var_list, str1,  Ast.Stat (stat), str2 ) =obji in
        begin 
            let (e1,e2) = print_intermid stat !defs_dic in  
            defs_dic := !defs_dic @ [(name, (e1,e2))];
            if name  = spec_stat_name then  print_string "" (* Ignor the last definition Spec *)
            else  
                let result=    
                if name = typeOk_stat_name then  
                    let  final_res=(type_Ok stat) in 
                    let string_type_dec= 
                        if List.length final_res.type_dec = 0 then ""
                        else
                            ("type string_type =") ^ (String.concat " | " final_res.type_dec) 
                    in 
                   (string_type_dec) ^ ("\n ") ^ (String.concat "" final_res.array_dec  ) ^ ("\n")
                else if name = init_stat_name then  
                    let vars_init= if  List.length var_list != 0 then  String.concat " " var_list
                                  else "z"  in 
                ("init (") ^ (vars_init) ^  (") {") 
                                            ^ (print_init stat !defs_dic) ^ ("} \n") 
                else if name =next_stat_name then 
                            print_Next stat !defs_dic name
                else if name = safety_stat_name then 
                        let vars_safe= if  List.length var_list != 0 then  String.concat " " var_list
                                  else "z"  in
                        let stat_without_not_eq = remove_not_eqal  stat !defs_dic in         
                        ("unsafe (")  ^ (vars_safe) ^  (") {") ^ ("not (") ^(stat_without_not_eq)^(" )")  ^ ("} \n")

                else "" 
                in  print_string result  ;
                final_result :=  !final_result ^ result; 
               
       end;                                                           
      done; 
        let oc = open_out "output.cub" in 
        Printf.fprintf oc "%s" (!final_result);  
        close_out oc;    
    end
   
            
  

   
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s  



(*              let oc = open_out "output.cub" in 
               Printf.fprintf oc "%s" (exp1 ^ "  == " ^ cub_string );  
               close_out oc;  *)




