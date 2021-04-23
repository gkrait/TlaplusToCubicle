-------------------------- MODULE distrib_channels --------------------------
EXTENDS Integers, Sequences, TLC, FiniteSets
CONSTANTS Tick,  Proc

VARIABLES State, Num, Aux, Channel_msg, Channel_v


Init ==  /\  State= [obj \in Proc |-> "Idle"]
         /\  Num= [obj \in Proc |-> 0]   (* In Cubicle the type of the intitial values were float. Do you think it is fine to make it integer?  *)
         /\  Aux= [obj \in Proc |-> 0]   (* The same problem is here  *) 
         /\ Channel_msg= [obj1, obj2 \in Proc |-> "Empty" ] 
         /\ Channel_v= [obj1, obj2 \in Proc |-> 1 ]  (* In the original Cubicle spec, it was: Channel_v[z,z1] = 1.0 && Channel_v[z,z1] = 0.0 ... I guess this is a typo ?   *)

Safety == \A z1, z2 \in Proc : (\/ State[z1] # "Use"
                               \/ State[z2] # "Use"
                               \/ Channel_msg[z1,z2] # "Ok2"
                               \/  Channel_msg[z2,z1] # "Ok2")
                               /\ 0 < Tick
                               /\ z1 # z2
                               
                               
t1(z) == /\ State= [obj \in Proc |-> "Idle"]
         /\ State'= [State EXCEPT ![z] = "Choose" ]
         /\ Aux' =   [Aux EXCEPT ![z] = Num[z]]    
         /\ \A r ,s \in Proc : Channel_msg[s,r]' = CASE  (/\ s=z  /\ r # z  /\ Channel_msg[s,r] = "Empty") -> "Req1"
                                                      [] OTHER -> Channel_msg[s,r]  
         /\ \A r ,s \in Proc : Channel_v[s,r]' = CASE  (/\ s=z /\ r # z  /\ Channel_msg[s,r] = "Empty") -> Num[z]
                                             [] OTHER -> Channel_v[s,r]  
         /\  UNCHANGED Aux                                    
                                             
 
t2(z,r) ==  /\ State[z] = "Choose"
            /\ Channel_msg[z,r] = "Ack1"
            /\ Aux[z] < Channel_v[z,r]
            /\ Channel_msg[z,r] = "Ok1"
            /\ Channel_msg'=[Channel_msg EXCEPT ![z,r] = "OK1" ]
            /\ Aux'=[Aux EXCEPT ![z] = Channel_v[z,r] ]
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num
            

t3(z,r)==   /\ State[z] = "Choose"
            /\ Channel_msg[z,r] = "Ack1"
            /\ Channel_v[z,r] <=   Aux[z] 
            /\ Channel_msg'=[Channel_msg EXCEPT ![z,r] = "OK1" ] 
            /\  UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num
            
            
            

t4(z) ==    /\ State[z] = "Choose"
            /\ 0 < Tick
            /\ \A r \in Proc: Channel_msg[z,r] = "Ok1" 
            /\ State' =   [State EXCEPT ![z] = "Wait"]
            /\ Num' =   [Num EXCEPT ![z] =Aux[z] + Tick]  
            /\ UNCHANGED Aux 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Channel_msg 



            
            



t5(z,s)==   /\ Channel_msg[s,z] = "Req1"
            /\ Channel_msg'=[Channel_msg EXCEPT ![s,z] = "Ack1" ] 
            /\ Channel_v'=[Channel_msg EXCEPT ![s,z] =  Num[z] ] 
            /\ UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Num
                        



t6(z,s) ==  /\ Channel_msg[s,z] = "Req2"
            /\ Channel_msg'=[Channel_msg EXCEPT ![s,z] = "Ack2" ] 
            /\ Channel_v'=[Channel_msg EXCEPT ![s,z] =  Num[z] ] 
            /\ UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Num
            


t7(z)==     /\ State[z] = "Wait"
            /\ \A r ,s \in Proc : Channel_msg[s,r]' = CASE  (/\s=z  /\ r # z  /\ Channel_msg[s,r] = "Empty") -> Num[z]
                                             [] OTHER -> Channel_v[s,r]  
            /\ UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num                                 
 
t8(z,r) ==   /\ State[z] = "Wait"
            /\ Channel_msg[z,r] = "Ack2"
            /\ 0.0 < Channel_v[z,r]
            /\ Channel_v[z,r] < Num[z]
            /\ Channel_msg'=[Channel_msg EXCEPT ![z,r] = "Req2" ] 
            /\  UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num


t8_2(z,r) == /\ State[z] = "Wait"
            /\ Channel_msg[z,r] = "Ack2"
            /\ 0 < Channel_v[z,r]
            /\ Channel_v[z,r] < Num[z]
            /\ r < z
            /\ Channel_msg'=[Channel_msg EXCEPT ![z,r] = "Req2" ] 
            /\  UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num



t9_1(z,r)== /\ State[z] = "Wait"
            /\ Channel_msg[z,r] = "Ack2"
            /\  Channel_v[z,r]=0
            /\ Channel_msg'=[Channel_msg EXCEPT ![z,r] = "Ok2" ] 
            /\  UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num


t9_2(z,r)== /\ State[z] = "Wait"
            /\ Channel_msg[z,r] = "Ack2"
            /\  Channel_v[z,r] > Num[z]
            /\ Channel_msg'=[Channel_msg EXCEPT ![z,r] = "Ok2" ] 
            /\  UNCHANGED Aux 
            /\ UNCHANGED State 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num


t10(z)==    /\ State[z] = "Wait"
            /\  \A r \in Proc : Channel_msg[z,r] = "Ok2" 
            /\  State'= [State EXCEPT ![z] = "Use" ]
            /\  UNCHANGED Aux 
            /\ UNCHANGED Channel_msg 
            /\ UNCHANGED Channel_v 
            /\ UNCHANGED Num


t11(z)==    /\ State[z] = "Use"
            /\  State'= [State EXCEPT ![z] = "Idle" ]
            /\  Num'= [Num EXCEPT ![z] = 0 ]
            /\ \A r ,s \in Proc : Channel_msg[s,r]' = CASE  (/\s=z  /\ r # z  /\ Channel_msg[s,r] = "Ok2") -> " Empty"
                                             [] OTHER -> Channel_v[s,r]
            /\  UNCHANGED Aux       
            /\ UNCHANGED Channel_v 
                                   
                                             
                                             

            


Next == \E z , r  : (\/  t1(z) \/ t2(z,r) \/  t3(z,r) \/ t4(z) \/ t5(z,r) \/ t6(z,r) \/ t7(z) \/ t8(z,r) \/ t8_2(z,r)  \/ t9_1(z,r) \/ t9_2(z,r) \/ t10(z) \/ t11(z)  )

Spec == Init \/ [][Next]_<<State, Num, Aux, Channel_msg, Channel_v>>


=============================================================================
