%!TEX program = lualatex
------------------------------ MODULE FragGram ------------------------------
( * We use  BNFGrammars https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/Syntax/BNFGrammars.tla * ) 



Spec == Init & tok("\/") & tok("[][") & Next & tok("]_") & VARIABLES


VARIABLES == tok("<<")  
             &(  Identifier & ( tok(",") & Identifier )^*  ) 
             & tok(">>")


(* ############################################################################################ *)
(* ########### Defining the Init part ######################################################### *)
(* ############################################################################################ *)

Init == (tok('/\') & Simple_Propositional_Exp )^+
Simple_Propositional_Exp == (Identifier & tok("=") & VALUE)   
                             |  (Identifier & tok("\in") & Finite_Set)

Finite_Set == tok('{') 
                & Value & ( tok(",") & Value)^*
                &  tok('}') 
              | Array 


(* In the following, VALUE is meant to be a closed proposition *)
VALUE == Identifier (* I have a constant in mind *) 
         | Numeral^+ 
         | STRING 
         | Numeral^+ & tok(".") & Numeral^*
         | Boolean   
         | Function
         | Array 
         | Identifier  & tok("[") & (TERM) & tok("]")
         | Identifier  & tok("(") & (TERM) & tok(")")
          


(* ################################################ *)
(* ########### Defining Functions ################# *)
(* ################################################ *)

(* Algorithmic issue to be discussed with Stephan:
     As agreed before, a Function is intended to be mainly translated w.r.t
     the Cubicle procedures. However, I still think that it is possible 
     and useful to consider Arrays as I define them. This can be done if 
     we limit the number  process (number_procs). The maximal number of 
     processes is determined by the lengths of arrays and functions in the TLA+ Spec  *)


Function == tok(“[”)     
                & Identifier 
                & tok(" \in ") 
                & (Identifier | Array) 
                & tok(“|->”) 
                & TERM
                & tok(“]”) 




(* In the following, Tok(P) means that the set of all terms of the form P  *)
STRING == Tok (tok(" " ") & NameChar^* & tok(" " ") )  \ Tok(ReservedWord) 


Array ==  Numeral^+ & tok("..") & Numeral^+
          | Numeral^+ & tok("..") &  Identifier
          | Identifier & tok("..") &  Numeral^+
          | Identifier & tok("..") &  Identifier

Identifier == Name \ Tok(ReservedWord)
Name == Tok((NameChar^* & Letter & NameChar^*))
NameChar  == Letter \cup Numeral \cup {"_"}  
Letter == OneOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
Numeral   == OneOf("0123456789") 







(* ############################################################################################ *)
(* ########### Defining the Next part ######################################################### *)
(* ############################################################################################ *)

Next ==  (tok('\/') & Next_State_Exp)^+




Next_State_Exp ==    (tok('/\') & Predicate)^*
                  &  (tok('/\') & Primed_Exp)^+

(* ################################################ *)
(* ########### Defining Predicate ################# *)
(* ################################################ *)
 
 Predicate ==  Propositional_Exp
               |  tok("(") 
                  &( (tok('\E') | tok('\A')) 
                  & Identifier
                  & tok("\in")
                  & (Identifier | Array) 
                  & tok(' : ') 
                  &  Predicate
                  & tok(")")
                  



(.* Fix and make it simpler *)
Propositional_Exp ==  Simple_Propositional_Exp 
                    | tok("(") & TERM & OPERATOR & TERM) & tok(")")
                    | tok('~') & tok("(")   & Propositional_Exp & tok(")")
                    | tok("(")   
                       & Propositional_Exp  
                       & Logical_Junctions
                       & Propositional_Exp
                       & tok(")")


\* GK: what about now? 



OPERATOR == tok("=")  |  tok("#") | tok("~=") | tok("<")  | tok("<=")


Logical_Junctions == tok("/\") | tok("\/")


TERM == VALUE | Open_Prpos



Open_Prpos == Identifier    
             | Identifier  & tok("[") & (TERM) & tok("]")
             | Identifier  & tok("(") & (TERM) & tok(")")
             (* generalize it more? *)
             

            

(* ################################################ *)
(* ########### Defining Primed_Exp ################ *)
(* ################################################ *)


Primed_Exp ==   (Identifier 
                & tok(' ' ') 
                & tok("=")  
                & (TERM | Function_Except    )^+



             
Function_Except == tok("[") &  Identifier  & tok("EXCEPT") 
                   & (tok('![') &  Identifier & tok(']=') & TERM )^+  
                   & tok("]")





