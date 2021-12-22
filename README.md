

``` TlaplusToCubicle ``` is a software to translate a spec written in a fragment of [TLA++](https://lamport.azurewebsites.net/tla/tla.html)  producing an 
equivalent spec  in [Cubicle](http://cubicle.lri.fr/) input language. Such a translator is useful for TLA+ specs that are defined by parametrized transition systems with states represented as arrays indexed by an arbitrary number of processes. For checking this type of classes, [Cubicle](http://cubicle.lri.fr/) model checker shows good results.          


# Assumptions and Limitations
We assume that the input spec is written by a TLA+ fragment described by the grammer defined by a [Menhir-generated parser](https://github.com/gkrait/TlaplusToCubicle/blob/master/Ast_builder/parser.mly).    

# Example
To translate the  [Berkeley](https://github.com/gkrait/TlaplusToCubicle/blob/master/Examples/Berkeley/input.in) spec described below, 
we save it in a .tla file  foo.tla



```
-------------------------- MODULE Berkeley --------------------------
EXTENDS  TLC
CONSTANTS  Proc
VARIABLES A 
TypeOk == A \in [Proc ->  { " Idle "  , " Wait " , " Crit ",  " Crash " } ] 
Init ==  A = [obj \in Proc |-> "Idle"] 
Safety(z1,z2) ==   (~(A[z1] = Crit) 
                \/ ~(A[z2] = Crit))
                /\ (z1 # z2) 
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
```
Then, we run the following coomands:  

```
make
./Tla2Cubicle <foo.tla>
```
The file output.cub is then generated containing the translation of foo.tla content:
```
type string_type =Idle | Wait | Crit | Crash
 array A[proc] : string_type 

init (z) {A[z]=Idle} 
unsafe (z1 z2) {not ((not (A[z1] = Crit)) || (not (A[z2] = Crit)) )} 

transition Next_1 (z) 
requires {
A[z] = Idle}
{
A[j] := case 
| j = z : Wait 
| j  <  z : A[j] 
| ( z  <  j ) && ( A[j] = Idle ) : A[j] 
| _ : Crash ; 
}

transition Next_2 (z) 
requires {
A[z] = Wait}
{
A[j] := case 
| j = z : Crit 
| j  >  z : A[j] 
| ( z  >  j ) && ( A[j] = Idle ) : Idle 
| _ : Crash ; 
}

transition Next_3 (z) 
requires {
A[z] = Crit}
{
A[j] := case | j = z : Idle | _ :  A[j]; }

```
