 
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
    
let rec print_log oper= match oper with 
 | Cubicle_tree.Conj ->  (" && ")  
 | Cubicle_tree.Disjun ->  " || "


let print_inq inq = match inq with 
 | Cubicle_tree.EQ -> "="
 | Cubicle_tree.Greater -> ">"
 | Cubicle_tree.Less -> "<"
 | Cubicle_tree.Inclus -> "\\in"

let rec print_prop prop= match prop with 
  | Cubicle_tree.Equality (exp1, exp2) -> if exp1 = exp2 then "trivial equality" else 
                               print_exp exp1 ^      "[z]=" ^      print_exp exp2  (*^(let x= (exp1 = exp2) in 
                                                              (str_of_bool x)  ) *)  
  | Cubicle_tree.Inequality (exp1, inq ,  exp2) ->    "(" ^ print_exp exp1^ "[z]" ^ 
                     (print_inq inq ) ^ print_exp exp2 ^ ")"
         
  | Cubicle_tree.UNCHAN -> "UNCHAN"   
  | Cubicle_tree.Coposition (prop1, oper, prop2) -> (print_prop prop1) ^ (print_log oper) ^    (print_prop prop2) 
     (* let r1 = begin match prop1 with 
     | Cubicle_tree.Equality(e1,e2) ->  if e1=e2  then  "0" else "1" 
     | Cubicle_tree.UNCHAN -> "0"
     | _ -> "1" end  in    
           
     let r2 = begin match prop2 with 
     | Cubicle_tree.Equality(e3,e4) ->  if e3=e4  then  "0" else "1" 
     | Cubicle_tree.UNCHAN -> "0"
     | _ -> "1" end  in   
begin 
     if r1^r2="00" then ""
     else if  r1^r2="10" then print_prop prop1
     else if  r1^r2="01" then print_prop prop2
   else (print_prop prop1) ^ (print_log oper) ^    (print_prop prop2) 
end  *)
     
      
   

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
let rec sublist l i j =
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j)
let rec print_temp_sim temp = match temp with 
 | Cubicle_tree.Pred (Cubicle_tree.Prop prop) -> (print_prop prop)  
 | Cubicle_tree.Primed primed_equality -> (print_primed primed_equality)


let rec list_to_str temp_list = let l= List.length temp_list in 
                                   match l with 
                                       | 1 -> let ele_temp= (List.nth  temp_list 0) in begin
                                         match ele_temp with 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.Equality(e1,e2) ))->  if e1=e2  then  "0" else print_temp_sim  ele_temp
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.UNCHAN  )) -> "0" 
                                          | _ -> print_temp_sim  ele_temp               end 
                                        
                                       | _ ->  begin  let  h::t= temp_list in 
                                         let r1 = begin match h with 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.Equality(e1,e2) ))-> 
                                            if e1=e2  then  "0" else "1" 
                                          | Cubicle_tree.Pred (Cubicle_tree.Prop( Cubicle_tree.UNCHAN  )) -> "0"
                                          | _ -> "1" end  in     
                                           if  r1="0" then  list_to_str (sublist temp_list 1 (l-1) )
                                          else    print_temp_sim (List.nth  temp_list 0) ^ " && " ^
                                              list_to_str (sublist temp_list 1 (l-1) ) end 


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


let rec print_temp name temp =  
  match temp with 
  | Cubicle_tree.Temp_Combination(temp1 , logicalop , temp2) -> begin match logicalop with 
   | Cubicle_tree.Conj ->  let class_list= (classifier temp) in 
     (* let zz= print_int (List.length  ( ( class_list.predi ) )) in *)
     let x1=  (list_to_str (remove_unchanged    class_list.predi ) ) in 
     (* let zz= print_string (("\n") ^ (x1)^("\n")) in *)
     (*let zz= print_string (("\n") ^ (x1)^("\n")) in  
      let zz= print_string "\n" in   *)
     let x2= (list_to_str class_list.pri  ) in 
     ("transition " ^ name ^ (begin incr ind ; string_of_int !ind ; end   ) ^ " (z) \nrequires { ")  ^ x1 ^ ( "} 
          {") ^ x2  ^ ("}\n") 
   | Cubicle_tree.Disjun ->  
      (print_temp name temp1) ^ (print_temp name temp2)  
      end 
  | Cubicle_tree.Pred (Cubicle_tree.Prop prop) -> ("transition " ^ name  ^ " (z) 
              \nrequires { ")  ^ (print_prop prop) ^ ( "} \n" ) 
  | Cubicle_tree.Primed primed_equality ->  ("{") ^ (print_primed primed_equality)  ^ ("}\n")









              


