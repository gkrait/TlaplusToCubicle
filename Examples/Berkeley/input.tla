-------------------------- MODULE Berkeley --------------------------
EXTENDS  TLC
CONSTANTS  Proc
VARIABLES A 
TypeOk == A \in [Proc ->  { "Idle"  , "Wait" , "Crit",  "Crash" } ] 
Init ==  A = [obj \in Proc |-> "Idle"] 
Safety(z1,z2) ==   z1 # z2 => (~(A[z1] = "Crit") \/ ~(A[z2] = "Crit")) 
tr1(z) ==  A[z] = "Idle"
           /\ A' = [ j \in Proc |-> CASE  j=z -> "Wait"
                                           [] j < z -> A[j] 
                                           []  z<j /\ A[j]="Idle" ->  A[j]
                                           [] OTHER -> "Crash" ] 

tr2(z) ==  A[z] = "Wait"
          /\ A' = [ j \in Proc |-> CASE  j=z -> "Crit"
                                           [] j> z -> A[j] 
                                           []  z>j /\ A[j]="Idle" -> "Idle"
                                           [] OTHER -> "Crash" ]                              
tr3(z) ==  A[z] = "Crit"
          /\ A'=[A EXCEPT ![z]="Idle"]

Next== \E z \in Proc :  tr1(z)
                       \/ tr2(z)
                       \/ tr3(z)  

     
===============================
