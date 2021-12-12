 
 type temp_conjunctions = {predi : Cubicle_tree.temp list  ; pri : Cubicle_tree.temp list } 


let print_op op = match op with
  | Cubicle_tree.Add ->  "+"
  | Cubicle_tree.Sub ->  "-"
  | Cubicle_tree.Mul ->  "*"
  | Cubicle_tree.Div ->  "/"

let  rec str_of_bool b = if b then "\n true \n" else "\n false \n"
let rec print_exp exp = match exp with
  | Cubicle_tree.Var v -> v 
  | Cubicle_tree.INT  (num)->  num  
  | Cubicle_tree.Func_img (var1,var2) -> print_exp var1 ^ "[" ^ print_exp var2 ^ "]" 
  | Cubicle_tree.STRING str -> str
  | Cubicle_tree.Binop (exp0, op, exp1) ->
           print_exp exp0  ^      print_op op ^      print_exp exp1  
  | Cubicle_tree.TRUE -> "true"  
  | Cubicle_tree.FALSE -> "false"              
    
let rec print_log oper= match oper with 
 | Cubicle_tree.Conj ->  (" && ")  
 | Cubicle_tree.Disjun ->  " || "


let print_inq inq = match inq with 
 | Cubicle_tree.EQ -> "="
 | Cubicle_tree.Greater -> ">"
 | Cubicle_tree.Less -> "<"
 | Cubicle_tree.Inclus -> "\\in"

let rec print_prop prop defs_dic= match prop with  
  | Cubicle_tree.Equality (exp1, exp2) -> if exp1 = exp2 then "trivial equality" else 
                               print_exp exp1 ^      "[z]=" ^      print_exp exp2  (*^(let x= (exp1 = exp2) in 
                                                              (str_of_bool x)  ) *)  
  | Cubicle_tree.Inequality (exp1, inq ,  exp2) ->    "(" ^ print_exp exp1^ "[z]" ^ 
                     (print_inq inq ) ^ print_exp exp2 ^ ")"       
  | Cubicle_tree.UNCHAN -> "UNCHAN"   
  | Cubicle_tree.Coposition (prop1, oper, prop2) -> (print_prop prop1 defs_dic) ^ (print_log oper) ^    (print_prop prop2 defs_dic) 
  | Cubicle_tree.Declaration(e1,e2,e3) -> "\n Declaration \n"
  | Cubicle_tree.Not_equal (exp1, exp2) -> 
                               print_exp exp1 ^      "[z] # " ^      print_exp exp2  
  | Cubicle_tree.Open_prop (prop_name,  var_list) -> 
      let l= List.length defs_dic 
      and substitution = ref ("","") in
      for i= 0 to l-1 do 
        let (def_name,(def_stat,e2))=List.nth defs_dic i in 
        if prop_name = def_name then  substitution := (def_stat,e2)
        else substitution := !substitution; 
      !substitution;
      done;
      let (e1,e2) = !substitution in 
      (e1)^ ("\n")  ^ (e2) 
      

                

     
      
   

 let rec print_equality cub_obj = match cub_obj with 
   | Cubicle_tree.ELEstat(str,stat)   -> begin
       match str with 
         | "unprime function equality integer" -> "hi"
   end
 
let rec e4_deal exp = match exp with 
 | Cubicle_tree.Var v -> v ^"[z]" 
 |  Cubicle_tree.Binop(e1,op,e2) -> (e4_deal e1) ^ (print_op op ) ^ (e4_deal e2)
 | _ -> print_exp exp   


let rec print_primed primed_stat = match primed_stat with 
    | Cubicle_tree.Primed_assig (exp1,exp2) -> print_exp exp1 ^ " := " ^ print_exp exp2 
    | Cubicle_tree.Cases(e1,e2,e3,e4) ->  
      print_exp e1 ^ "[j] := case | j = z"   ^ " : " ^
       
       e4_deal e4
        ^    
          " | _ :  " ^ print_exp e2 ^ "[j]; " 

 let rec classifier temp = match temp with 
 | Cubicle_tree.Pred pred ->  {predi   = [Cubicle_tree.Pred(pred)] ; pri = [] }
 | Cubicle_tree.Primed primed -> {predi   = [] ; pri = [Cubicle_tree.Primed primed]}
 | Cubicle_tree.Temp_Combination (temp1 , logicalop , temp2) -> let list1= classifier (temp1) in 
                                                                let list2= classifier (temp2) in
                                { predi = (list1.predi) @ (list2.predi) ;
                                  pri = list1.pri @ list2.pri }
let rec sublist l i j defs_dic=
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j defs_dic)
let rec print_temp_sim temp defs_dic= match temp with 
 | Cubicle_tree.Pred (Cubicle_tree.Prop prop) -> (print_prop prop defs_dic)  
 | Cubicle_tree.Primed primed_equality -> (print_primed primed_equality)


let rec list_to_str temp_list defs_dic= let l= List.length temp_list in 
                                   match l with 
                                       | 0 -> ""
                                       | 1 -> begin let ele_temp= (List.nth  temp_list 0) in 
                                         match ele_temp with 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.Equality(e1,e2) ))->  
                                                  if e1=e2  then  "0" else print_temp_sim  ele_temp defs_dic
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.UNCHAN  )) -> "0" 
                                          | _ -> print_temp_sim  ele_temp defs_dic            end 
                                        
                                       | _ ->  begin  let  h::t= temp_list in 
                                         let r1 = begin match h with 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.Equality(e1,e2) ))-> 
                                                  if e1=e2  then  "0" else "1" 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.UNCHAN  )) -> "0"
                                          | _ -> "1" end  in     
                                           let sublist_string_tail = sublist temp_list 1 (l-1) defs_dic in 
                                           if  r1="0" then 
                                             list_to_str sublist_string_tail defs_dic
                                          else    (print_temp_sim (List.nth  temp_list 0) (defs_dic)) ^ (" && ") ^
                                             (list_to_str sublist_string_tail defs_dic) end 


let rec  remove_unchanged props_list = 
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


let ind= ref 0 


let rec print_temp name temp defs_dic=  
  match temp with 
  | Cubicle_tree.Pred (Cubicle_tree.Prop prop) ->  [(print_prop prop defs_dic,"") ] 
  | Cubicle_tree.Primed primed_equality -> [ ("",print_primed primed_equality) ]
  | Cubicle_tree.Negation pred  -> let l = print_temp  name pred defs_dic  in 
      let result= ref [] in 
          for i=0 to ((List.length l)-1) do  
            let (e1,e2) = List.nth l i in 
            result:= !result @ [(("~") ^ ("(") ^ (e1) ^ (")")  , 
                                ("~") ^ ("(") ^ (e2) ^ (")") )]
          done;
          !result 
  | Cubicle_tree.Open_temp (temp_name,  var_list) -> 
      let l= List.length defs_dic 
      and substitution = ref ("","") in
      for i= 0 to l-1 do 
        let (def_name,(pred_part,primed_part))=List.nth defs_dic i in 
        if temp_name = def_name then  substitution := (pred_part,primed_part)
        else substitution := !substitution; 
      !substitution;
      done;
      [!substitution ]
  | Cubicle_tree.Temp_Combination(temp1 , logicalop , temp2) -> begin match logicalop with 
    | Cubicle_tree.Conj ->  let class_list= (classifier temp) in 
      let x1=  (list_to_str (remove_unchanged    class_list.predi ) defs_dic) in 
      let x2= (list_to_str class_list.pri defs_dic ) in 
       [(x1 , x2)]  
    | Cubicle_tree.Disjun ->  
     let list1= (print_temp name temp1 defs_dic) 
     and  list2 = (print_temp name temp2 defs_dic)  in 
     list1 @ list2
      end 








              


