Init == A=[x \\in Proc |-> 1];
one == 1;
Invar== x <10 ;
Next(x)==  A[x]'=A[x]+one;
Spec== Init \\/ \\A x \\in Proc : Next(x) ;  

