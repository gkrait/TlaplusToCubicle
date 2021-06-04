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

Finite_Set == tok('{') & Value & ( tok(",") & Value)^*
                                &  tok('}')


(* In the following, VALUE is ment to be a closed proposition *)
VALUE == Constant | Numeral^+ | STRING 
         | Numeral^+ & tok("...") $ Numeral^+
         | Numeral^+ & tok(".") & Numeral^*
         | Boolean   
         | Function  

Function == tok(“[”)   
                & Identifier & tok(" \in ") &  Identifier 
                & tok(“|->”) & TERM)
                & tok(“]”) 


Identifier == Name \ Tok(ReservedWord)
Name == Tok((NameChar^* & Letter & NameChar^*))
NameChar  == Letter \cup Numeral \cup {"_"}  
Letter == oneof("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
Numeral   == OneOf("0123456789") 



(* ############################################################################################ *)
(* ########### Defining the Next part ######################################################### *)
(* ############################################################################################ *)

Next ==  (tok('\/') & Next_State_Exp)^+




Next_State_Exp ==    (tok('/\') Predicate)^*
                  &  (tok('/\') Primed_Exp)^+

(* ################################################ *)
(* ########### Defining Predicate ################# *)
(* ################################################ *)
 
 Predicate == (tok('\/') & Propositional_Exp)^*
              |  tok('\/')  
                 & ( (tok('\E') | tok('\A')) & VARIABLE )^+ 
                 & tok(' : ') 
                 &  Propositional_Exp 




Propositional_Exp == tok('(') & (tok('/\') & Simple_Propositional_Exp )^* & tok(')')
                    | tok('(') &  (tok('/\') & TERM & OPERATOR & TERM)^* & tok(')')
                    | tok('(') & (tok('/\') & tok('~') & Propositional_Exp)^* & tok(')')

\* sm: Note that you have to start with "/\". In particular, one has to write
\* sm: something like "/\ ~ /\ x > 0", which is a little awkward.
\* GK: what about now? 


OPERATOR == tok("=")  |  tok("#") | tok("~=") | tok("<")  | tok("<=")


TERM == VALUE | Open_Prpos



Open_Prpos == Identifier    
             | (Identifier & tok("[") &  (VALUE | Identifier) & tok("]"))
             
             

(* ################################################ *)
(* ########### Defining Primed_Exp ################ *)
(* ################################################ *)


Primed_Exp == (tok('/\') & VARIABLE &   tok(' ' ') & tok("=")  & TERM )^+
              
              | (tok('/\') & VARIABLE &   tok(' ' ') &  tok("=") & Function )^+

              | (tok('/\') & VARIABLE &   tok(' ' ') &  tok("=") & Function_Except )^+



             
Function_Except == tok("[") &  (Identifier| VALUE)  & tok("EXCEPT") 
                   & (tok('![') &  Identifier & tok(']=') & TERM )^+  
                   & tok("]")





