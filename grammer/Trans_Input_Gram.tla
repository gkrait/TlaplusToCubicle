------------------------------ MODULE FragGram ------------------------------
( * We use  BNFGrammars https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/Syntax/BNFGrammars.tla * ) 



Spec == Init \/ [][Next]_<<VARIABLES>>

Init == (tok('/\') & Simple_Propositional_Exp )^+


Next ==  (tok('\/') & Next_State_Exp)^+




Simple_Propositional_Exp == (VARIABLE & tok("=") & VALUE)   
                            
                             |  (VARIABLE & tok("\in") & Finite_Set)


Finite_Set == tok('{') & Simple_Value & (Simple_Value & tok(","))^*
                                &  tok('}')


VALUE == Constant | Closed_Expression






Next_State_Exp ==    (tok('/\') Predicate)^*
                  &  (tok('/\') Primed_Exp)^+




Predicate == (tok('\/') & Propositional_Exp)^*
             |  tok('\/') & 
                            ( (tok('\E') | tok('\A')) & VARIABLE )^+ & 
                                                                    Propositional_Exp
              

Propositional_Exp == (tok('/\') & Simple_Propositional_Exp )^* 
                    & (tok('/\') & TERM & OPERATOR & TERM)^*
                    & (tok('/\') & tok('~') & Propositional_Exp)^*


OPERATOR == tok("=")  |  tok("<>") |  tok("<")  | tok("<=")



TERM == VALUE | VARIABLE  | (VARIABLE & tok("[") &  Proc_Identifier & tok("]"))


Primed_Exp == (tok('/\') & VARIABLE &   tok(' ' ') & tok("=")  & TERM )^+
              
              | (tok('/\') & VARIABLE &   tok(' ' ') &  tok("=") & Record )^+

              | (tok('/\') & VARIABLE &   tok(' ' ') &  tok("=") & Record_Except )^+




Record == tok("[") &  
                     (Proc_Identifier & tok('\in') & Proc_SubSet  )^+ & tok('|->') 
                                                                            & TERM & tok("]")


 Record_Except == tok("[") &  Record & tok("EXCEPT") & 
                 (tok('!') &  Proc_Identifier & tok('=') & TERM)^+ & tok('|->') 
                  & TERM & tok("]")
                     
                                                                                  
