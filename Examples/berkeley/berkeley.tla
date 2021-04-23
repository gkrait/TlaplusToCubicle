EXTENDS Integers, Sequences, TLC, FiniteSets
CONSTANTS  Proc
VARIABLES A

Safety == \A z1 , z2 \in Proc : (\/  A[z1] # "Crit"  
                                \/  A[z2] # "Crit")
                                /\ z1 #z2



Init ==  A = [obj \in Proc |-> "Idle"]


tr1(z) == /\ A[z] = "Idle"
          /\ \A j \in Proc :  A[j]' = CASE  j=z -> "Wait"
                                           [] j < z -> A[j] 
                                           [] /\ z<j /\ A[j]="Idle" ->  A[j]
                                           [] OTHER -> "Crash"



tr2(z) == /\ A[z] = "Wait"
          /\ \A j \in Proc :  A[j]' = CASE  j=z -> "Crit"
                                           [] j> z -> A[j] 
                                           [] /\ z>j /\ A[j]="Idle" -> "Idle"
                                           [] OTHER -> "Crash"


tr3(z) == /\ A[z] = "Crit"
          /\ A'=[A EXCEPT ![z]="Idle"]


Next== \E z \in Proc : \/ tr1(z)
                       \/ tr2(z)
                       \/ tr3(z)

Spec == Init \/ [][Next]_<<A>>


=============================================================================

