
------------------------------ MODULE FragGram ------------------------------
( * We use  BNFGrammars https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/Syntax/BNFGrammars.tla * ) 



Spec == Init 
        & tok("\/") 
        & tok("[][") 
        & Next 
        & tok("]_") 
        & VARIABLES


VARIABLES == tok("<<")  
             & ( Identifier 
             & (tok(",") & Identifier)^* ) 
             & tok(">>")


(* ############################################################################################ *)
(* ########### Defining the Init part ######################################################### *)
(* ############################################################################################ *)

Init == (tok('/\') & Simple_Propositional_Exp)^+
Simple_Propositional_Exp ==  (Identifier & tok("=") & VALUE)   
                             |  (Identifier & tok("\in") & Finite_Set)

Finite_Set == tok('{') 
                & Value 
                & (tok(",") & Value)^*
                &  tok('}') 
              | Numeral^+ & tok("..") & Numeral^+
              | Numeral^+ & tok("..") &  Identifier
              | Identifier & tok("..") &  Numeral^+
              | Identifier & tok("..") &  Identifier  
 

VALUE == Identifier (* I have a constant in mind *) 
         | Numeral^+ 
         | Numeral^+ & tok(".") & Numeral^*
         | STRING 
         | Boolean   
         | Function
         | Identifier  & tok("[") & (TERM) & tok("]")
         | Identifier  & tok("(") & (TERM) & tok(")")
          



Function == tok(“[”)     
                & Identifier 
                & tok(" \in ") 
                & Identifier  (* I have a subset of Proc in mind*)
                & tok(“|->”) 
                & TERM
                & tok(“]”) 




(* In the following, Tok(P) means that the set of all terms of the form P  *)
STRING == Tok (tok(" " ") & NameChar^* & tok(" " ") )  \ ReservedWords (* The same ReservedWords defined in TLAPlusGrammar.tla *)

Boolean == tok("True") | tok("False")



Identifier == Name \ ReservedWords
Name == Tok((NameChar^* & Letter & NameChar^*))
NameChar  == Letter \cup Numeral \cup {"_"}  
Letter == OneOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
Numeral   == OneOf("0123456789") 







(* ############################################################################################ *)
(* ########### Defining the Next part ######################################################### *)
(* ############################################################################################ *)

Next ==  (tok('\/') & Next_State_Exp)^+




Next_State_Exp ==    (tok('/\') & Predicate)^*
                     & (tok('/\') & Primed_Exp)^+

(* ################################################ *)
(* ########### Defining Predicate ################# *)
(* ################################################ *)
 
 Predicate ==  Propositional_Exp         
               |  tok("(") 
                  & (tok('\E') | tok('\A'))  
                  & Identifier (* For the moment, let us stay in the one-variable case *)
                  & tok("\in")
                  & Identifier  (* I have a subset of Proc in mind *)
                  & tok(' : ') 
                  & Predicate
                  & tok(")")
                  




Propositional_Exp ==  Simple_Propositional_Exp 
                      | tok("(") & TERM & OPERATOR & TERM & tok(")")
                      | tok('~') & tok("(")   & Propositional_Exp & tok(")")
                      | tok("(")   
                        & Propositional_Exp  
                        & Logical_Junctions
                        & Propositional_Exp
                        & tok(")")


OPERATOR == tok("=")  |  tok("#") | tok("~=") | tok("<")  | tok("<=")


Logical_Junctions == tok("/\") | tok("\/")


TERM == VALUE | Open_Prpos



Open_Prpos == Identifier    
             | Identifier  & tok("[") & TERM & tok("]")
             | Identifier  & tok("(") & TERM & tok(")")
             (* generalize it more? *)
             

            

(* ################################################ *)
(* ########### Defining Primed_Exp ################ *)
(* ################################################ *)


Primed_Exp ==   Identifier 
                & tok(' ' ') 
                & tok("=")  
                & (TERM | Function_Except)



             
Function_Except == tok("[") 
                   &  Identifier  
                   & tok("EXCEPT") 
                   & ( tok('![') &  Identifier & tok(']=') & TERM 
                     & (tok('![') &  Identifier & tok(']=') & TERM )^+ ) 
                   & tok("]")




