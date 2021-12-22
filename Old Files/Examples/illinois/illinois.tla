EXTENDS Integers, Sequences, TLC, FiniteSets
CONSTANTS  Proc
VARIABLES A

Safety == \A z1 , z2 \in Proc : (\/  A[z1] # "L3"  
                                \/  A[z2] # "L2")
                                /\ z1 #z2

Init ==  A = [obj \in Proc |-> "L1"]


t1(x, y) == /\ A[x] = "L1"
            /\ A[y] = "L2" 
            /\ \A j \in Proc :  A[j]' = CASE  \/ j = x \/  y=j  -> "L3"
                                              [] OTHER -> A[j]
 

t2(x) == /\ A=[y\in Proc |-> "L1" ] 
         /\  A' = [A EXCEPT ![x]="L4"] 




t3(x) == /\ A[x]="L1"
         /\ \A j \in Proc :  A[j]' = CASE j = x -> "L2"
                                          [] A[j] \in {"L2", "L3","L4"} -> "L1"
                                          [] OTHER -> A[j] 



t4(x) ==  /\ A[x]="L1" 
          /\  A' = [A EXCEPT ![x]="L1"] 


t5(x,y) == /\ A[x]="L1"
           /\ A[y]="L3"
           /\ \A j \in Proc :  A[j]' = CASE j = x -> "L3"
                                          [] A[j] = "L4" -> "L3"
                                          [] OTHER -> A[j] 


t5bis(x,y) == /\ A[x]="L1"
              /\ A[y]="L4"
              /\ \A j \in Proc :  A[j]' = CASE j = x -> "L3"
                                          [] A[j] = "L4" -> "L3"
                                          [] OTHER -> A[j] 


t6(x) ==   /\ A[x]="L3"
           /\ \A j \in Proc :  A[j]' = CASE j = x -> "L2"
                                          [] A[j] = "L3" -> "L1"
                                          [] OTHER -> A[j] 



t7(x) == /\ A[x]= "L3"
         /\  A' = [A EXCEPT ![x]="L1"] 

t8(x) == /\ A[x]= "L4"
         /\  A' = [A EXCEPT ![x]="L1"] 

t9(x) == /\ A[x]= "L4"
         /\  A' = [A EXCEPT ![x]="L2"] 



Next== \E x ,y \in Proc : \/ t1(x,y) 
                          \/ t2(x)
                          \/ t3(x)
                          \/ t4(x)
                          \/ t5(x,y)
                          \/ t5bis(x,y)
                          \/ t6(x)
                          \/ t7(x)
                          \/ t8(x)
                          \/ t9(x)
                       
                       

Spec == Init \/ [][Next]_<<A>>


=============================================================================

