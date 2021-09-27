 type temp_conjunctions = {tempo : Cubicle_tree.temp list  ; pri : Cubicle_tree.temp list } 

let print_op op = match op with
  | Cubicle_tree.Add ->  "+"
  | Cubicle_tree.Sub ->  "-"
  | Cubicle_tree.Mul ->  "*"
  | Cubicle_tree.Div ->  "/"


let rec print_exp exp = match exp with
  | Cubicle_tree.Var v -> v
  | Cubicle_tree.INT  (num)->  num  
  | Cubicle_tree.Func_img (var1,var2) -> print_exp var1 ^ "[" ^ print_exp var2 ^ "]"   
      

  | Cubicle_tree.Binop (exp0, op, exp1) ->
    
       "(" ^      print_exp exp0 ^      print_op op ^      print_exp exp1 ^       ")"
    



let rec print_compar compar = match compar with 
| Cubicle_tree.EQ ->  " = "
| Cubicle_tree.Greater ->  " > "
| Cubicle_tree.Less ->  " < "
| Cubicle_tree.Inclus ->  " \\in "


let rec print_log oper= match oper with 
| Cubicle_tree.Conj ->  " && "
| Cubicle_tree.Disjun ->  " || "



let rec print_prop prop= match prop with 
  | Cubicle_tree.Equality (exp1, exp2) -> print_exp exp1 ^      "=" ^      print_exp exp2     
  | Cubicle_tree.Inequality (exp1, exp2) ->    "[" ^ print_exp exp1 ^      "<" ^      print_exp exp2 ^ "]"
         
  | Cubicle_tree.Coposition (prop1, oper, prop2) -> 
         
      print_prop prop1 ^      print_log oper ^      print_prop prop2 

 let rec print_equality cub_obj = match cub_obj with 
   | Cubicle_tree.ELEstat(str,stat)   -> begin
       match str with 
         | "unprime function equality integer" -> "hi"
   end
 


let rec print_primed primed_stat = match primed_stat with 
    | Cubicle_tree.Primed_assig (exp1,exp2) -> print_exp exp1 ^ " := " ^ print_exp exp2 
    | Cubicle_tree.Cases(e1,e2,e3,e4) -> print_exp e1 ^ "[j] := case | j = " ^ print_exp e3 ^ " : " ^ print_exp e4 ^
          " | _ :  " ^ print_exp e2 ^ "[j];" 



 let rec classifier temp = match temp with 
 | Cubicle_tree.Pred pred ->  {tempo   = [Cubicle_tree.Pred(pred)] ; pri = [] }
 | Cubicle_tree.Primed primed -> {tempo   = [] ; pri = [Cubicle_tree.Primed primed]}
  | Cubicle_tree.Combination (temp1 , logicalop , temp2) -> let list1= classifier (temp1) in 
                                                          let list2= classifier (temp2) in
                                                          { tempo =  list1.tempo @ list2.tempo ; 
                                                            pri = list1.pri @ list2.pri }
let rec sublist l i j =
  if i > j then
    []
  else
    (List.nth l i) :: (sublist l (i+1) j)



let rec print_temp_sim temp = match temp with 
| Cubicle_tree.Pred (Cubicle_tree.Prop prop) -> print_prop prop 
| Cubicle_tree.Primed primed_equality -> print_primed primed_equality


let rec list_to_str temp_list = let l= List.length temp_list in 
                                   match l with 
                                       | 1 -> print_temp_sim (List.nth  temp_list 0)
                                       | _ ->  print_temp_sim (List.nth  temp_list 0) ^ " && " ^
                                              list_to_str (sublist temp_list 1 (l-1) )

let rec print_temp temp = match temp with 
| Cubicle_tree.Pred (Cubicle_tree.Prop prop) ->   print_prop prop 
| Cubicle_tree.Primed primed_equality -> print_primed primed_equality
| Cubicle_tree.Combination(temp1 , logicalop , temp2) -> begin match logicalop with 
   | Cubicle_tree.Conj ->  let class_list= (classifier temp) in 
     let x1= (list_to_str class_list.tempo  ) in 
     let x2= (list_to_str class_list.pri  ) in 
      (" transition Next (z) \n 
          requires { ")  ^ x1 ^ ( "} \n 
          {") ^   x2  ^ ("}\n") 
   | Cubicle_tree.Disjun ->    (print_temp temp1) ^  (print_log logicalop) ^ (print_temp temp2)
      end 








              


