------------------------------ MODULE FragGram ------------------------------
( * We use  BNFGrammars https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/Syntax/BNFGrammars.tla * ) 



Spec == Init \/ [][Next]_<<VARIABLES>>
\* sm: /\ rather than \/ (and in fact, should use "tok(...)")
\* sm: VARIABLES is not defined

Init == (tok('/\') & Simple_Propositional_Exp )^+


Next ==  (tok('\/') & Next_State_Exp)^+




Simple_Propositional_Exp == (VARIABLE & tok("=") & VALUE)   
                            
                             |  (VARIABLE & tok("\in") & Finite_Set)


Finite_Set == tok('{') & Simple_Value & (Simple_Value & tok(","))^*
                                &  tok('}')
\* sm: you want to change the order of "," and Simple_Value
\* sm: Simple_Value is not defined (why not just VALUE)


VALUE == Constant | Closed_Expression
\* sm: Constant and Closed_Expression are not defined





Next_State_Exp ==    (tok('/\') Predicate)^*
                  &  (tok('/\') Primed_Exp)^+




Predicate == (tok('\/') & Propositional_Exp)^*
             |  tok('\/') & 
                            ( (tok('\E') | tok('\A')) & VARIABLE )^+ & 
                                                                    Propositional_Exp
\* sm: insert ":" before final Propositional_Exp              

Propositional_Exp == (tok('/\') & Simple_Propositional_Exp )^* 
                    & (tok('/\') & TERM & OPERATOR & TERM)^*
                    & (tok('/\') & tok('~') & Propositional_Exp)^*
\* sm: I presume that the "&" at the beginning of each line should be "|"
\* sm: Note that you have to start with "/\". In particular, one has to write
\* sm: something like "/\ ~ /\ x > 0", which is a little awkward.

OPERATOR == tok("=")  |  tok("<>") |  tok("<")  | tok("<=")
\* sm: I am not sure that "<>" is legal TLA+. I always write "#", but
\* sm: I believe that "~=" also works.


TERM == VALUE | VARIABLE  | (VARIABLE & tok("[") &  Proc_Identifier & tok("]"))
\* sm: One-dimensional arrays only? OK with me for the moment.
\* sm: If you intend to support records, you should also allow "rec.field" here,
\* sm: and in fact you can have arrays of records ...

Primed_Exp == (tok('/\') & VARIABLE &   tok(' ' ') & tok("=")  & TERM )^+
              
              | (tok('/\') & VARIABLE &   tok(' ' ') &  tok("=") & Record )^+

              | (tok('/\') & VARIABLE &   tok(' ' ') &  tok("=") & Record_Except )^+




Record == tok("[") &  
                     (Proc_Identifier & tok('\in') & Proc_SubSet  )^+ & tok('|->') 
                                                                            & TERM & tok("]")
\* sm: What is Proc_SubSet? Why noy Proc_Identifier?
\* sm: In fact, "Record" is a bad name – these are functions, not records.

 Record_Except == tok("[") &  Record & tok("EXCEPT") & 
                 (tok('!') &  Proc_Identifier & tok('=') & TERM)^+ & tok('|->') 
                  & TERM & tok("]")
\* sm: I think "Record" should be "VARIABLE" or something like that.
\* sm: And in fact, the real syntax is "[array EXCEPT ![p] = 42, ![q] = 27]".
