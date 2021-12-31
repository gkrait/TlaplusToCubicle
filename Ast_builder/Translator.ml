type cub_declaration = {type_dec : string list  ; array_dec : string list } 

(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(*************************************   Useful Tools   *******************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)


let check_str s = 
  try int_of_string s |> ignore; "int"
  with Failure _ -> if (s ="FALSE" || s= "TRUE") then "bool" else "str"

let rec tla_to_cub_biop biop = match biop with 
 | Tla_Ast.Add -> Cubicle_tree.Add 
 | Tla_Ast.Sub -> Cubicle_tree.Sub 
 | Tla_Ast.Mul -> Cubicle_tree.Mul 
 | Tla_Ast.Div -> Cubicle_tree.Div 

let  rec tla_to_cub_comp biop = match biop with 
  | Tla_Ast.EQ -> Cubicle_tree.EQ 
  | Tla_Ast.Greater -> Cubicle_tree.Greater 
  | Tla_Ast.Less -> Cubicle_tree.Less 
  | Tla_Ast.Inclus -> Cubicle_tree.Inclus 

let rec tla_to_cub_exp(expr)= match expr with 
  | Tla_Ast.STRING(v) -> Cubicle_tree.STRING(v)
  | Tla_Ast.INT(n) -> Cubicle_tree.INT(n)
  | Tla_Ast.Func_img(e1,vars) -> Cubicle_tree.Func_img(tla_to_cub_exp e1,vars )
  | Tla_Ast.Func_def(e1,e2,e3) -> Func_def(tla_to_cub_exp e1, tla_to_cub_exp e2, tla_to_cub_exp e3)
  | Tla_Ast.Func_exception(e1,e2,e3) ->  tla_to_cub_exp e3 
  | Tla_Ast.TRUE -> Cubicle_tree.TRUE
  | Tla_Ast.FALSE -> Cubicle_tree.FALSE
  | Tla_Ast.Var(v) -> Cubicle_tree.Var(v)
  | Tla_Ast.Binop(e1, bio , e2) -> Cubicle_tree.Binop(tla_to_cub_exp(e1), tla_to_cub_biop bio , tla_to_cub_exp(e2)) 

let rec parse_def def = match def with 
    | Tla_Ast.Value (exp1, var_list,logicalop , exp2 , uni) -> Tla_Ast.ElE ( exp1 ,var_list, "value",  exp2, "not leaf" )  
    | Tla_Ast.Statment (exp1 ,var_list, logicalop , temp , uni) ->  Tla_Ast.ElE (exp1  , var_list, "statment" , temp, "not leaf" )  

let rec sublist l i j =
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j)

let rec parse_tla spec = match spec with 
    | Tla_Ast.File(Tla_Ast.VARI(var), Tla_Ast.CONS(con),fil) ->  begin 
        match fil with 
        | Tla_Ast.MulDef (defns_list) -> begin  let n=(List.length defns_list) in 
            match  n  with 
                | 0 -> []
                | 1 ->   parse_def (List.nth defns_list 0)  ::[]
                | _ ->   let sub= (sublist defns_list 1 (n-1) )  in 
                         let img_sub=    parse_tla  (Tla_Ast.File(Tla_Ast.VARI (var),
                                Tla_Ast.CONS(con) , Tla_Ast.MulDef (sub)  )) in 
                    let head = [parse_def (List.nth defns_list 0 )] in 
                        head @ img_sub;
                 end
        | _  -> []            end
    | _  -> []              


(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(***********************************     Translating       ****************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)

let rec trans_equality  eq  = let Tla_Ast.Equality(l1  ,l2) =eq   in 
  match l1 with 
    | Tla_Ast.Func_img  ( Tla_Ast.Var x1 , vars)  ->  
     begin  
       match l2 with 
                 | Tla_Ast.Func_def(x,proc, value) ->  let xx= tla_to_cub_exp x and procp= tla_to_cub_exp proc
                 and value_tr= tla_to_cub_exp value in 
                  Cubicle_tree.ELEstat("", 
                  Cubicle_tree.Equality ( tla_to_cub_exp l1, Cubicle_tree.Func_def(xx,procp, value_tr)  ) )
                 | Tla_Ast.Var(v) -> Cubicle_tree.ELEstat ("unprime  equality var", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.Var(v) ) )
                 | Tla_Ast.INT num-> Cubicle_tree.ELEstat ("unprime  equality int", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.INT(num) ) ) 
                 | Tla_Ast.Binop (exp1 , bi , exp2)-> Cubicle_tree.ELEstat ("Uncovered unprime mix exp", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.Var ("z") ) )                                                                    
                                                   
                 | Tla_Ast.STRING(s) ->   Cubicle_tree.ELEstat (" unprime  equality string", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1 , Cubicle_tree.STRING (s) ) ) 
                 | Tla_Ast.TRUE ->   Cubicle_tree.ELEstat (" unprime  equality bool", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.TRUE  ) )                                                             
                 | Tla_Ast.FALSE ->   Cubicle_tree.ELEstat (" unprime  equality bool", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.FALSE ) )                           
                 | Tla_Ast.Func_img(e1,e2) -> let e1_cub= tla_to_cub_exp e1 in 
                    Cubicle_tree.ELEstat ("unprime  equality", 
                                                  Cubicle_tree.Equality(tla_to_cub_exp l1, Cubicle_tree.Func_img(e1_cub,e2) ) )
                 | Tla_Ast.Func_exception(exp1 , exp2 , exp3) -> 
                    Cubicle_tree.ELEstat ("prime  equality", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var("Uncovered case "),
                                                   Cubicle_tree.Var("  ") ) )                                                             
                                             
                                             end 



                                       
  | _ -> Cubicle_tree.ELEstat ("prime  equality", 
                                                  Cubicle_tree.Equality(Cubicle_tree.Var("Uncovered case "),
                                                   Cubicle_tree.Var("  ") ) )                                                                                                   

let rec change_log_type log= match log with 
 | Tla_Ast.Conj -> Cubicle_tree.Conj
 | Tla_Ast.Disjun -> Cubicle_tree.Disjun

let rec trans_prop prop = match prop with 
    | Tla_Ast.Equality (exp1  , exp2) ->  (trans_equality (Tla_Ast.Equality (exp1  , exp2))) 
    | Tla_Ast.Inequality (exp1 , com , exp2) ->      let  Cubicle_tree.ELEstat ( str, Cubicle_tree.Equality(l1,l2)) = (trans_equality (Tla_Ast.Equality (exp1  , exp2))) in 
                                                    Cubicle_tree.ELEstat ("unprime  inequality", 
                                                    Cubicle_tree.Inequality(l1,tla_to_cub_comp com ,l2)  )
 |  Tla_Ast.Open_prop (v,var_list) ->  
                Cubicle_tree.ELEstat("open prop", Cubicle_tree.Open_prop(v,var_list))
 | Tla_Ast.Not_equal(exp1,exp2) -> 
    Cubicle_tree.ELEstat ("not equality", 
    Cubicle_tree.Not_equal(tla_to_cub_exp exp1 , tla_to_cub_exp exp2 ))
 | Tla_Ast.Inclusion(exp , string_list ) -> 
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
                                
 | Tla_Ast.UNCHAN (str) ->  Cubicle_tree.ELEstat( "proposition", 
                          Cubicle_tree.UNCHAN)

 | Tla_Ast.Coposition  (prop1 , logicalop , prop2) ->  let  Cubicle_tree.ELEstat (str1,stat1) =  trans_prop  prop1  in 
                                                 let   Cubicle_tree.ELEstat (str2,stat2)=  trans_prop  prop2 in
                                                 let log = change_log_type logicalop in 
                                                  Cubicle_tree.ELEstat("proposition", Cubicle_tree.Coposition(stat1,log ,stat2) )  

let  rec tla_to_cub_log biop = match biop with 
    | Tla_Ast.Conj -> Cubicle_tree.Conj 
    | Tla_Ast.Disjun -> Cubicle_tree.Disjun
    | Tla_Ast.ASSIG -> Cubicle_tree.ASSIG  

let rec trans_pred pred =
 match pred with 
   | Tla_Ast.Prop prop ->  trans_prop prop
   | Tla_Ast.Universal (quanti , var_list  , Tla_Ast.Var(z)  , pred1)  -> begin
            match pred1 with 
                | Tla_Ast.Prop pro -> begin match  pro with 
                       |  Tla_Ast.Equality  (exp1  , exp2)  ->
                            let l1=tla_to_cub_exp exp1 in 
                            let l2=tla_to_cub_exp exp2 in 
                            Cubicle_tree.ELEstat ("unprime  equality",
                                    Cubicle_tree.Equality(l1 , l2)  )

                       | Tla_Ast.Inequality (exp1 , coparism , exp2) -> 
                                let l1=tla_to_cub_exp exp1 in 
                                let l2=tla_to_cub_exp exp2 in 
                                Cubicle_tree.ELEstat ("unprime  inequality",
                                    Cubicle_tree.Inequality(l1, tla_to_cub_comp coparism,l2)  )

                        |  Tla_Ast.Not_equal  (exp1  , exp2)  ->
                           let l1=tla_to_cub_exp exp1 in 
                            let l2=tla_to_cub_exp exp2 in 
                            Cubicle_tree.ELEstat ("unprime  not equal",
                                    Cubicle_tree.Not_equal(l1 , l2 )  )
                        | Tla_Ast.Coposition(pr1,log,pr2) ->
                            let  Cubicle_tree.ELEstat(strq,tra_pr1)=  trans_prop pr1 in 
                            let  Cubicle_tree.ELEstat(str2 ,tra_pr2)=  trans_prop pr2 in    
                            Cubicle_tree.ELEstat(  "unprime composition"  ,
                            Cubicle_tree.Coposition(tra_pr1, tla_to_cub_log log ,tra_pr2) )
                        | _ ->   Cubicle_tree.ELEstat(  "UNCOVERED"  ,
                          Cubicle_tree.Equality( Cubicle_tree.Func_img(Cubicle_tree.Var("v") ,
                             ["z"] )  ,
                              Cubicle_tree.Var("v") )  )       end 

                 | Tla_Ast.Pred_Comp(pr1,log,pr2) ->
                        let  Cubicle_tree.ELEstat(strq,tra_pr1)=  trans_pred pr1 in 
                        let  Cubicle_tree.ELEstat(str2 ,tra_pr2)=  trans_pred pr2 in    
                            Cubicle_tree.ELEstat(  "unprime composition"  ,
                          Cubicle_tree.Coposition(tra_pr1, tla_to_cub_log log,tra_pr2) ) (* George check here *)  end
   | Tla_Ast.Pred_Comp(pred1 , logicalop , pred2) -> 
                 let  Cubicle_tree.ELEstat(str1  ,  prop1) = trans_pred  pred1 in 
                 let  Cubicle_tree.ELEstat(str2  ,  prop2) = trans_pred  pred2 in 
                 let logi=tla_to_cub_log logicalop in 
                Cubicle_tree.ELEstat ("Mix", 
                Cubicle_tree.Coposition(prop1, logi ,prop2))
   | Tla_Ast.Existence (quanti , var_list , Tla_Ast.Var(z)  , pred1) -> 
           trans_pred  pred1
        (*Cubicle_tree.ELEstat ("Uncovered", 
                Cubicle_tree.Equality(Cubicle_tree.Var("x") , Cubicle_tree.Var("x"))) *)
   | Tla_Ast.Open_pred(stat_name, var_list) ->  Cubicle_tree.ELEstat ("open pred", 
                Cubicle_tree.Open_prop(stat_name, var_list))           
 

         (* Cubicle_tree.ELEstat ( "To Check", trans_pred pred1)
       Cubicle_tree.Equality ( Cubicle_tree.Var("xnxnxnn"), 
          Cubicle_tree.Var(z) ) )  *)

let rec drop_str info= match info with 
 |Cubicle_tree.ElEassig(str, stat) -> Cubicle_tree.Primed(stat) 
 |Cubicle_tree.ELEstat(str, stat )  -> Cubicle_tree.Pred (Cubicle_tree.Prop  stat )
 | Cubicle_tree.ELEMix(str,stat) -> stat

let rec tla_to_cub_arrow (arrows)= match arrows with
     | [arrow] ->  let  Tla_Ast.Arrow(prop,exp) = arrow in 
            let exp_cub= tla_to_cub_exp exp 
            and info_prop_cub = trans_pred  (Tla_Ast.Prop prop) in 
            let Cubicle_tree.ELEstat(str,prop_cub)= info_prop_cub in 
            [Cubicle_tree.Arrow(prop_cub,exp_cub)]
    | h::t -> tla_to_cub_arrow ([h]) @ tla_to_cub_arrow (t)        

let rec trans_temp obj =   match obj with 
 | Tla_Ast.Predec pred  ->  trans_pred pred
 | Tla_Ast.Func_except (exp1,var_list,Tla_Ast.Func_exception(exp2,Tla_Ast.Func_img(Tla_Ast.Var exp3,[]),exp4 ) )  -> 
        Cubicle_tree.ElEassig ("primed equality except" ,  
                Cubicle_tree.Primed_assig( Cubicle_tree.Func_img( tla_to_cub_exp exp1,[exp3] ),
                                     tla_to_cub_exp  exp4     )      ) 
 |Tla_Ast.Prime (exp1 , string_list , exp2 ) ->   Cubicle_tree.ElEassig ("primed equality" , 
      Cubicle_tree.Primed_assig(tla_to_cub_exp exp1 ,tla_to_cub_exp exp2 )) 
 | Tla_Ast.Mix (temp1, logicalop , temp2) -> let trans1 = trans_temp temp1 in 
                                        let trans2 = trans_temp temp2 in
                                       Cubicle_tree.ELEMix( "mix" , 
                                        Cubicle_tree.Temp_Combination (drop_str trans1 , tla_to_cub_log logicalop ,drop_str trans2 )  )
 | Tla_Ast.Open_temp (stat_name,var_list) -> 
        Cubicle_tree.ELEMix("open temp",
          Cubicle_tree.Open_temp(stat_name,var_list)  )
 | Tla_Ast.Negation temp ->  let Cubicle_tree.ELEMix(str, trans) = trans_temp temp in  
 Cubicle_tree.ELEMix("negation",
          Cubicle_tree.Negation(trans)  )  
 | Tla_Ast.CASES(exp , vars , arrows ) ->  let cub_arrows= tla_to_cub_arrow arrows in 
       Cubicle_tree.ELEMix("Cases",
        Cubicle_tree.CASES(Cubicle_tree.Func_img((tla_to_cub_exp exp) , vars), vars , cub_arrows )  )   
  | Tla_Ast.Implication(temp1,temp2) -> let equiv= Tla_Ast.Mix(Tla_Ast.Negation(temp2), Tla_Ast.Disjun, temp1 ) in 
   trans_temp equiv

(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(*******************************  Computing negation of statement  ********************************)
(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)

let rec negation_prop stat = match stat with 
    | Tla_Ast.Equality(e1,e2) -> Tla_Ast.Not_equal(e1,e2)
    | Tla_Ast.Not_equal(e1,e2) -> Tla_Ast.Equality(e1,e2)
    | Tla_Ast.Coposition(e1,log,e2) -> 
            begin match log with 
                | Tla_Ast.Conj -> Tla_Ast.Coposition(negation_prop e1, Tla_Ast.Disjun, negation_prop e2 )
                | Tla_Ast.Disjun -> Tla_Ast.Coposition(negation_prop e1, Tla_Ast.Conj, negation_prop e2 )
            end
    | Tla_Ast.UNCHAN vars ->   stat
    | Tla_Ast.Open_prop(name,vars) -> stat   (* will fix that in translation *)      

let rec negation_pred stat = match stat with 
    | Tla_Ast.Prop (prop) -> Tla_Ast.Prop (negation_prop prop)
    | Tla_Ast.Existence (Tla_Ast.Exis, vars, Tla_Ast.Var v, (pred)) -> 
        Tla_Ast.Universal(Tla_Ast.Univ,vars,Tla_Ast.Var v, negation_pred stat) 
    | Tla_Ast.Universal (Tla_Ast.Univ, vars, Tla_Ast.Var v, pred) -> 
        Tla_Ast.Existence (Tla_Ast.Exis, vars, Tla_Ast.Var v, negation_pred (pred)) 

let rec negation_temp stat = match stat with
    | Tla_Ast.Predec(pred) -> Tla_Ast.Predec (negation_pred pred)  
    | Tla_Ast.Mix(temp1,log,temp2) ->   begin match log with 
                | Tla_Ast.Conj -> Tla_Ast.Mix(negation_temp temp1, Tla_Ast.Disjun, negation_temp temp2 )
                | Tla_Ast.Disjun -> Tla_Ast.Mix(negation_temp temp1, Tla_Ast.Conj, negation_temp temp2 )
            end 
    | Tla_Ast.Implication(temp1,temp2) ->  Tla_Ast.Mix(negation_temp temp1,Tla_Ast.Conj, temp2)
    | Tla_Ast.Negation(temp) -> temp         

(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(*******************************  Printing Output File   ******************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)


let rec print_Cubobj  name cub_obj defs_dic = match cub_obj with 
    | Cubicle_tree.ELEstat(str,stat ) -> begin  match str with 
           | "equality integer" -> [( (Cub_print.fix_print_prop_output(Cub_print.print_prop  stat defs_dic "Next") ),"")]
           | _ -> [(Cub_print.fix_print_prop_output (Cub_print.print_prop  stat defs_dic "Next") ,"")]
         end 
    | Cubicle_tree.ElEassig (str, assig) -> [("", Cub_print.print_primed  assig )]   
    | Cubicle_tree.ELEMix (str, stat ) ->  (Cub_print.print_temp  stat defs_dic "Next"  )                    

let rec type_Ok  def= match def with 
  |   Tla_Ast.Predec(Tla_Ast.Prop(prop)) -> begin  
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
                   | _ -> { type_dec = [] ; array_dec = [] }  
                                                                       end 
            |  Cubicle_tree.Coposition(prop1, logicalop , prop2  )  -> 
             let Tla_Ast.Coposition(prop1, logicalop , prop2  ) = prop in 
                let res1 = type_Ok  (Tla_Ast.Predec (Tla_Ast.Prop (prop1))) in 
                let res2 =  type_Ok (Tla_Ast.Predec (Tla_Ast.Prop (prop2))) in
                 { type_dec= res1.type_dec @ res2.type_dec   ;  array_dec = res1.array_dec @ res2.array_dec  }
            | _ -> { type_dec = [] ; array_dec = ["Uncovered Case"] }       end 
 | Tla_Ast.Mix(temp1,log,temp2)  -> 
        let res1 = type_Ok  temp1 in 
        let res2 = type_Ok  temp2 in 
    { type_dec= res1.type_dec @ res2.type_dec   ;  array_dec = res1.array_dec @res2.array_dec  }  
 | _ -> { type_dec= []   ;  array_dec = []  }  

let rec non_trivial_con(str1,str2, sep)= 
    if str1 = "" && str2 = "" then ""
    else if  str1 = "" && str2 != "" then str2
    else if str1 != "" && str2 = "" then str1
    else let lpar= if (String.contains_from str1 0 ':') && (String.contains_from str1 0 '=')  then "" else "(" 
            and  rpar= if (String.contains_from str1 0 ':') && (String.contains_from str1 0 '=') then "" else ")" in 
         let sep_general= if  lpar = "(" then  sep else "" in  
    lpar ^ (str1) ^ rpar ^ (sep_general) ^ lpar ^ (str2) ^ rpar

let rec print_init init_stat defs_dic= match init_stat with 
    | Tla_Ast.Predec( init_pred) -> let  stat_cub= trans_pred init_pred in 
        let  Cubicle_tree.ELEstat (str, prop_cub ) = stat_cub in 
        let  (stat_str,e2) = (( (Cub_print.print_prop prop_cub defs_dic "init") )) in
          stat_str ^e2
    | Tla_Ast.Mix(tem1,logic , tem2) -> 
         ("(") ^ ((print_init tem1 defs_dic) ) ^ (")") 
        ^ ( Cub_print.print_log (change_log_type logic) )
        ^ ("(") ^ (print_init tem2 defs_dic)  ^ (")")
    | Tla_Ast.Negation pred -> ("not ") ^("(") ^(print_init pred defs_dic) ^(")")
    | Tla_Ast.Open_temp (prop_name,  var_list) -> 
        let l= List.length defs_dic 
        and substitution = ref ("","") in
        for i= 0 to l-1 do 
            let (def_name, (def_stat, e,open_vars))=List.nth defs_dic i in 
            if prop_name = def_name then  substitution := (def_stat, e)
            else substitution := !substitution;
            !substitution;
        done;
        let (e1,e2) = !substitution in 
        e1
    | Tla_Ast.Implication(pred1, pred2) -> let eq_stat=  Tla_Ast.Mix(negation_temp(pred2), Tla_Ast.Disjun, pred1) in 
              print_init  eq_stat defs_dic    
    | _ -> ""    

let rec remove_not_eqal safty_stat defs_dic = match safty_stat with 
    | Tla_Ast.Implication (Tla_Ast.Predec( Tla_Ast.Prop (Tla_Ast.Not_equal(Tla_Ast.Func_img(exp1,[]) ,Tla_Ast.Func_img(exp2,[]))) ) ,
        temp2)->  print_init (negation_temp temp2) defs_dic 
    | Tla_Ast.Predec( Tla_Ast.Prop (Tla_Ast.Not_equal(Tla_Ast.Func_img(exp1,[]) ,Tla_Ast.Func_img(exp2,[]))) ) -> ""
    | Tla_Ast.Mix(tem1,log, tem2) -> 
        let stat1= remove_not_eqal tem1 defs_dic 
        and  stat2= remove_not_eqal tem2 defs_dic in 
        let log_str = match log with 
                |Tla_Ast.Conj -> " && " 
                |Tla_Ast.Disjun -> " || "
                | _ -> ""  in 
        non_trivial_con(stat1,stat2,log_str)
    | Tla_Ast.Negation(temp) -> (remove_not_eqal (negation_temp temp) defs_dic)    
    | _ ->   print_init (negation_temp safty_stat) defs_dic                       

let rec print_intermid ?(open_def_vars="")  intermid_stat defs_dic = match intermid_stat with 
        | Tla_Ast.Predec (pred) ->  let  stat_cub= trans_pred pred in 
        let  Cubicle_tree.ELEstat (str, prop_cub ) = stat_cub in
        let  (stat_str,e2) = (Cub_print.print_prop   prop_cub defs_dic "Next" ~open_def_vars:open_def_vars)  in
          (stat_str ^e2,"")
        | (Tla_Ast.Prime(exp1 , var_last , exp2)) | (Tla_Ast.Func_except(exp1 , var_last , exp2)) ->
            let stat_trans = trans_temp intermid_stat in
            let Cubicle_tree.ElEassig(info_str, trans_info)= stat_trans in
            let trans_str= Cub_print.print_primed trans_info in 
            ("",trans_str)
        | Tla_Ast.Negation pred -> let (e1,e2) = print_intermid  pred  defs_dic  ~open_def_vars:open_def_vars in 
          (("~") ^ ("(") ^ (e1) ^ (")")  , 
                                ("~") ^ ("(") ^ (e2) ^ (")") ) (* George: I need to recheck here *)
        | Tla_Ast.Open_temp(name ,var_list) -> 
            let l= List.length defs_dic 
            and substitution = ref ("","") in
            for i= 0 to l-1 do 
                let (def_name, (pred_part,prim_part, open_vars))=List.nth defs_dic i in 
                if name = def_name then   substitution := (pred_part,prim_part) 
                else substitution := !substitution; 
                !substitution;
                done;
                !substitution 
        | Tla_Ast.Mix(temp1,log, temp2) -> 
            let log_str = match log with 
                |Tla_Ast.Conj -> " && " 
                |Tla_Ast.Disjun -> " || " 
                | _ -> "" 
            in     
            let (pred1,prim1) =  print_intermid  temp1 defs_dic  ~open_def_vars:open_def_vars
            and (pred2,prim2) =  print_intermid  temp2 defs_dic ~open_def_vars:open_def_vars in 
            (non_trivial_con(pred1, pred2, log_str) ,
                non_trivial_con(prim1,prim2, log_str) )  
        | Tla_Ast.CASES (exp , vars , cub_arrows ) -> 
            let  info_cub_stat=  trans_temp intermid_stat in  
            let  Cubicle_tree.ELEMix(str,cub_stat)=info_cub_stat in 
             let [(e1,e2)]= (Cub_print.print_temp cub_stat defs_dic "Next") in 
             (e1,e2)
        | Tla_Ast.Implication(pred1, pred2) -> let eq_stat=  Tla_Ast.Mix(negation_temp(pred2), Tla_Ast.Disjun, pred1) in 
              print_intermid  eq_stat defs_dic ~open_def_vars:open_def_vars

let rec print_Next next_stat defs_dic name var_list = let  tra=  trans_temp next_stat in 
    let transitions_str =  ref "" in 
    let stats_list= print_Cubobj  name tra defs_dic in 
    let l= List.length  stats_list in 
    let vars_next= if  List.length var_list != 0 then  String.concat " " var_list
                                  else "z"  in 
    for i =0 to l-1 do
    let (e1,e2) = List.nth stats_list i  in 
    if (e1,e2) = ("", "") then transitions_str := !transitions_str ^  ""
    else 
        transitions_str := !transitions_str ^ ("\ntransition ") ^ 
            (name) ^ ("_") ^(string_of_int (i+1)) ^ (" ("^ vars_next ^" ) \n") ^ ("requires {\n") ^
            (e1) ^("}\n") ^ ("{\n") ^(e2) ^("}\n")
    done;
    !transitions_str   

(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(***************************************  Main Function  ******************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)

let rec translate ?okx:(typeOk_stat_name= "TypeOk")
                  ?saf:(safety_stat_name="Safety") 
                  ?ini:(init_stat_name= "Init")   
                  ?nex:(next_stat_name= "Next") 
                  ?spc:(spec_stat_name= "Spec")
                  fil  = match fil with
    | Tla_Ast.File(Tla_Ast.VARI (var), Tla_Ast.CONS(con),defs) ->
        let  obj_list=   parse_tla fil in 
        let  l= List.length obj_list in 
        let defs_dic = ref [("",("","",[]))] in    
        begin
        let final_result =ref "" in
        for i= 0 to l-1 do 
        let  obji= List.nth  obj_list i in 
        match obji with 
            | Tla_Ast.ElE (DEFIN name ,var_list, str1,  Tla_Ast.Stat (stat), str2 ) ->
                begin 
                let (e1,e2) = print_intermid stat ~open_def_vars:( "[" ^ String.concat "," var_list ^ "]") !defs_dic in  
                defs_dic := !defs_dic @ [(name, (e1,e2,var_list))];
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
                            print_Next stat !defs_dic name var_list
                    else if name = safety_stat_name then 
                        let vars_safe= if  List.length var_list != 0 then  String.concat " " var_list
                                  else "z"  in
                        let stat_without_not_eq = if (List.length var_list) > 1 then  remove_not_eqal  stat !defs_dic
                            else print_init stat !defs_dic in         
                        ("unsafe (")  ^ (vars_safe) ^  (") {")  ^(stat_without_not_eq)^(" ")  ^ ("} \n")

                    else "" 
                in  print_string result  ;
                final_result :=  !final_result ^ result;   
                        end;
            | _ -> () 
                                                                  
      done; 
        let oc = open_out "output.cub" in 
        Printf.fprintf oc "%s" (!final_result);  
        close_out oc;    
        end
    | _ -> ()   
     
let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s  




