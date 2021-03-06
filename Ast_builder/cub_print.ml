 
 type temp_conjunctions = {predi : Cubicle_tree.temp list  ; pri : Cubicle_tree.temp list } 

(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(******************************   Printing Cubicle Objects   **************************************)
(**************************************************************************************************)
(**************************************************************************************************)
(**************************************************************************************************)

let rec print_op op = match op with
  | Cubicle_tree.Add ->  "+"
  | Cubicle_tree.Sub ->  "-"
  | Cubicle_tree.Mul ->  "*"
  | Cubicle_tree.Div ->  "/"
let rec str_of_bool b = if b then "\n true \n" else "\n false \n"
let rec print_exp exp = match exp with
  | Cubicle_tree.Func_def(e1,e2,e3) ->  "Function"
  | Cubicle_tree.Func_img (exp1,vars) -> let vars_str= if List.length vars = 0 then "" else  "[" ^(String.concat "," vars) ^"]" in 
  (print_exp exp1) ^  (vars_str)    
  | Cubicle_tree.STRING str -> str
  | Cubicle_tree.Binop (exp0, op, exp1) ->
           print_exp exp0  ^      print_op op ^  print_exp exp1  
  | Cubicle_tree.TRUE -> "True"  
  | Cubicle_tree.FALSE -> "False" 
  | Cubicle_tree.Var v ->  v
  | Cubicle_tree.INT  (num)->  num        
let rec print_log oper= match oper with 
  | Cubicle_tree.Conj ->  (" && ")  
  | Cubicle_tree.Disjun ->  " || "
let rec  print_inq inq = match inq with 
 | Cubicle_tree.EQ -> "="
 | Cubicle_tree.Greater -> ">"
 | Cubicle_tree.Less -> "<"
 | Cubicle_tree.Inclus -> "\\in"
let rec print_prop ?(open_def_vars="") prop defs_dic stat_position  = match prop with  
  | Cubicle_tree.Equality (exp1, exp2) ->  
   begin  
      match exp2 with 
      | Cubicle_tree.Func_def(e1,e2,e3) -> 
            if stat_position = "init" || stat_position = "unsafe" then 
                ((print_exp exp1) ^ ("[z]=") ^ (print_exp e3) ,"")
            else ((print_exp exp1)  ^(open_def_vars) ^ " = " ^ (print_exp e3) ^ " && forall_other j." 
              ^ (print_exp exp1)^ "[j] = " ^ (print_exp e3),"")

      | Cubicle_tree.Func_img(e,vars) -> 
       if (print_exp exp1) = (print_exp exp2) then ("","") 
        else  (print_exp exp1 ^ " = " ^  print_exp exp2 , "")
               
      | _ ->  ((print_exp exp1) ^ " = " ^ (print_exp exp2),"")
    end 
  | Cubicle_tree.Inequality (exp1, inq ,  exp2) ->   (  print_exp exp1 ^ 
                  "  "^   (print_inq inq ) ^ "  " ^ print_exp exp2 , "")       
  | Cubicle_tree.UNCHAN -> ("","")   
  | Cubicle_tree.Coposition (prop1, oper, prop2) ->  
    let (e1,e2) = (print_prop prop1 defs_dic stat_position) 
    and (e3,e4) = (print_prop prop2 defs_dic stat_position) in 
      ( ("( ") ^ e1 ^ (" )") ^ (print_log oper) ^ ("( ") ^ e3 ^ (" )") ,
         ("( ") ^ e2 ^ (" )") ^ (print_log oper) ^ ("( ") ^ e4 ^ (" )")  ) 
  | Cubicle_tree.Declaration(e1,e2,e3) -> ("\nDeclaration \n","")
  | Cubicle_tree.Not_equal (exp1, exp2) -> begin match exp2 with 
      | Cubicle_tree.Func_def(e1,e2,e3) -> (print_exp exp1 ^ "[z] <> " ^ print_exp e3,"") 
      | _ ->    (print_exp exp1 ^ " <> " ^ print_exp exp2,"")  end 
  | Cubicle_tree.Open_prop (prop_name,  var_list) -> 
      let l= List.length defs_dic 
      and substitution = ref ("","") in
      for i= 0 to l-1 do 
        let (def_name,(def_stat,e2,open_vars))=List.nth defs_dic i in 
        if prop_name = def_name then  substitution := (def_stat,e2)
        else substitution := !substitution; 
      !substitution;
      done;
      let (e1,e2) = !substitution in 
      (e1,e2) 
      

let rec fix_print_prop_output   print_prop_output= 
    let (e1,e2)= print_prop_output in e1               

let rec e4_deal exp = match exp with 
 | Cubicle_tree.Var v -> v ^"[z]" 
 |  Cubicle_tree.Binop(e1,op,e2) -> (e4_deal e1) ^ (print_op op ) ^ (e4_deal e2)
 | _ -> print_exp exp   
let rec print_primed primed_stat = match primed_stat with 
    | Cubicle_tree.Primed_assig (exp1,exp2) -> print_exp exp1 ^ " := " ^ print_exp exp2 ^ (";\n")
    | Cubicle_tree.Cases(e1,e2,e3,e4) ->  
      print_exp e1 ^ "[j] := case | j = z"   ^ " : " ^ e4_deal e4 ^ " | _ :  " ^ print_exp e2 ^ "[j]; " 

 let rec classifier temp stat_position= match temp with 
 | Cubicle_tree.Pred pred ->  {predi   = [Cubicle_tree.Pred(pred)] ; pri = [] } 
 | Cubicle_tree.Primed primed -> {predi   = [] ; pri = [Cubicle_tree.Primed primed]}
 | Cubicle_tree.Temp_Combination (temp1 , logicalop , temp2) -> let list1= classifier (temp1) stat_position in 
                                                                let list2= classifier (temp2) stat_position in
                                { predi = (list1.predi) @ (list2.predi); pri = list1.pri @ list2.pri }
 | Cubicle_tree.Open_temp(name, vars) -> { predi =[] ; pri = [temp]  }
 | _ ->  { predi =[] ; pri = [temp]  }                              
let rec sublist l i j defs_dic=
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j defs_dic)
let rec print_temp_sim temp defs_dic stat_position= match temp with 
 | Cubicle_tree.Pred (Cubicle_tree.Prop prop) -> 
 (fix_print_prop_output (print_prop prop defs_dic stat_position) )
 | Cubicle_tree.Primed primed_equality -> (print_primed primed_equality)
let rec list_to_str temp_list defs_dic stat_position= let l= List.length temp_list in 
                                   match l with 
                                       | 0 -> ""
                                       | 1 -> begin let ele_temp= (List.nth  temp_list 0) in 
                                         match ele_temp with 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.Equality(e1,e2) ))->  
                                                  if e1=e2  then  "0" else print_temp_sim  ele_temp defs_dic stat_position
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.UNCHAN  )) -> "0" 
                                          | _ -> print_temp_sim  ele_temp defs_dic stat_position           end 
                                        
                                       | _ ->  begin  let  h::t= temp_list in 
                                         let r1 = begin match h with 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.Equality(e1,e2) ))-> 
                                                  if e1=e2  then  "0" else "1" 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.UNCHAN  )) -> "0"
                                          | _ -> "1" end  in     
                                           let sublist_string_tail = sublist temp_list 1 (l-1) defs_dic in 
                                           if  r1="0" then 
                                             list_to_str sublist_string_tail defs_dic stat_position
                                          else    (print_temp_sim (List.nth  temp_list 0) (defs_dic) stat_position) ^ (" && ") ^
                                             (list_to_str sublist_string_tail defs_dic stat_position) end 
let rec remove_unchanged props_list = 
    let l= List.length props_list in 
    match l with
        | 1 -> let [Cubicle_tree.Pred( Cubicle_tree.Prop(ele))] = props_list in 
                 begin match ele with 
                      | Cubicle_tree.UNCHAN -> []
                      | Cubicle_tree.Equality(e1,e2) ->  if  e1=e2 then [] 
                                                            else props_list
                      | Cubicle_tree.Coposition( p1,l, p2) -> 
                        let an1= remove_unchanged [Cubicle_tree.Pred( Cubicle_tree.Prop(p1))] in 
                        let an2 = remove_unchanged [Cubicle_tree.Pred( Cubicle_tree.Prop(p2))] in 
                        if  List.length an1   != 0 && List.length an2 != 0 
                            then  an1 @ an2 
                        else if   List.length an1   = 0 && List.length an2 != 0
                            then an2 
                        else if   List.length an1   != 0 && List.length an2 = 0  
                            then an1 
                        else []
                      | _ -> props_list                                                                   
                 end                                                          
          | _ -> let h::t = props_list in 
                   remove_unchanged [h] @ remove_unchanged t
let rec print_arrows arrows defs_dic stat_position= match List.length arrows with
     | 1 ->  let arrow = List.nth arrows 0 in 
        let  Cubicle_tree.Arrow(prop,exp) = arrow in 
        ("| ") ^ (fix_print_prop_output (print_prop prop defs_dic stat_position)) ^ (" : ") ^ (print_exp exp) ^ (" \n")
    | _ -> let h= List.nth arrows 0 and t= List.nth arrows ((List.length arrows) -1  ) in 
            let result = ref (print_arrows [h] defs_dic stat_position) in  
            for i=1 to ((List.length arrows) -2  ) do 
              let ele= List.nth arrows i in 
                result := !result  ^(print_arrows [List.nth arrows i] defs_dic stat_position)  ; 
              done;  
              let  Cubicle_tree.Arrow(prop_t, exp_t)=t in  
              let prop_str = match  prop_t with 
                  | Cubicle_tree.Open_prop("OTHER",vars)-> "_ : "
                  | _ -> (fix_print_prop_output (print_prop prop_t defs_dic stat_position)) ^ (" : ") in 
             (!result)  ^ ("| ") ^  (prop_str) ^ (print_exp exp_t)   
let rec print_temp  temp defs_dic stat_position=  
  match temp with 
  | Cubicle_tree.Primed primed_equality -> [ ("",print_primed primed_equality) ]
  | Cubicle_tree.Pred (Cubicle_tree.Prop prop) ->   [print_prop prop defs_dic stat_position] 
  | Cubicle_tree.Negation pred  -> let l = print_temp pred defs_dic  stat_position in 
      let result= ref [] in 
          for i=0 to ((List.length l)-1) do  
            let (e1,e2) = List.nth l i in 
            result:= !result @ [(("not") ^ ("(") ^ (e1) ^ (")")  , 
                                ("not") ^ ("(") ^ (e2) ^ (")") )]
          done;
          !result 
  | Cubicle_tree.Open_temp (temp_name,  var_list) -> 
      let l= List.length defs_dic 
      and substitution = ref ("","") in
      for i= 0 to l-1 do 
        let (def_name,(pred_part,primed_part,open_vars))=List.nth defs_dic i in 
        if temp_name = def_name then  substitution := (pred_part,primed_part)
        else substitution := !substitution; 
      !substitution;
      done;
      [!substitution ]
  | Cubicle_tree.Temp_Combination(temp1 , logicalop , temp2) -> begin match logicalop with 
    | Cubicle_tree.Conj ->  let class_list= (classifier temp stat_position) in 
      let x1=  (list_to_str (remove_unchanged    class_list.predi  ) defs_dic stat_position) in 
      let x2= (list_to_str class_list.pri defs_dic stat_position) in 
       [(x1 , x2)]  
    | Cubicle_tree.Disjun ->  
     let list1= (print_temp  temp1 defs_dic stat_position) 
     and  list2 = (print_temp temp2 defs_dic stat_position)  in 
       list1 @ list2
    | _ -> []   
      end
  | Cubicle_tree.CASES(exp , vars , cub_arrows ) ->  ["",(print_exp exp ) ^ (" := case \n") ^ (print_arrows cub_arrows defs_dic stat_position) ^(" ; \n") ] 
  | _ -> []   
           

  (*| Cubicle_tree.CASES(vars , tla_to_cub_exp exp , cub_arrows )   -> [("","")] *)





              


