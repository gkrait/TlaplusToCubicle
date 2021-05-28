------------------------------ MODULE FragGram ------------------------------
(* Similar to [1,Sec 15.1] *)

EXTENDS Naturals, Sequences, BNFGrammars 
(* The following grammer defines a fragment of TLA++ for the purpose of designing a 
translation algorithm from that fragment into Cubicle input language.
Note that the grammer extends modules BNFGrammars. Hence, we consider  the following standard conventions:
 OneOf(s), tok(s), Tok(S), Nil, L & M, L | M,L^+, L^*. *) 


 
 (* We start by defining some sets of lexemes. First is ReservedWord, the set of words that can’t
  be used as identifiers. (Note that boolean, true, false, and string are identifiers that
   are predefined.) *)



ReservedWord == 
  {"ASSUME","UNION", "VARIABLE", "EXCEPT", "CASE", "EXTENDS", "CONSTANT", "CONSTANTS", "UNCHANGED",
  "IF", "THEN","ELSE","LET","DOMAIN"}

(* Next are three sets of characters—more precisely, sets of 1-character lexemes. 
They are the sets of letters, numbers, and characters that can appear in an identifier. *)

Letter == oneof("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
Numeral   == OneOf("0123456789") 
NameChar  == Letter \cup Numeral \cup {"_"}  

(*  We now define some sets of tokens. A Name is a token composed of letters, numbers, and
"_" characters that contains at least one letter, but does not begin with “WF_” or “SF_”. 
 It can be used as the name of a record field or a module. An Identifier is a Name that isn’t
  a reserved word.  An IdentifierOrTuple is either an identifier or a tuple of identifiers. *)


Name == Tok((NameChar^* & Letter & NameChar^*))
Identifier == Name \ Tok(ReservedWord)
IdentifierOrTuple  ==   
   Identifier | tok("<<") & CommaList(Identifier) & tok(">>")
  
(*  Now we define a float number. In contrast to [1, Sec 15.1], we restrict to the decimal representation?  *)  
NumberLexeme == 
         Numeral^+ 
      |  (Numeral^* & {"."} & Numeral^+) 
Number == Tok(NumberLexeme)


(* A String token represents a literal string. We follow the same rules presented in [1, Sec 16.1.10] of typing
 special characters in a string.  *)

String == Tok({"\""} & STRING & { "\"" })


(* We next define the sets of tokens that represent prefix operators. We consider a restricted sub-set of
 the one defined in [1,Sec 15.1]. *)

PrefixOp  ==  Tok({ "-", "~", "\not", "\neg",  "UNCHANGED",
"\leq","\geq"})
InfixOp == Tok({ "#", "+",  "-", "...",  "=",  "==", "^",  ">", "<", "\in" })
(* I did not find interpretation of many InfixOp symbols defined in [1, Sec 15.2]. I simply omitted them.   *) 
PostfixOp ==  Tok({ "^+", "^*", "^'" })

(* Now, de define the grammer, but before that we define two  operators  *)
  AtLeast4(s) == Tok({s \o s \o s} & {s}^+) 
CommaList(L) == L & (tok(",") &  L)^*



 LET P(G) ==
   /\ G.Module ::=   AtLeast4("-") & tok("MODULE") & Name & AtLeast4("-") 
                   & (Nil | (tok("EXTENDS") & CommaList(Name)))
                   & (G.Unit)^* 
                   & AtLeast4("=") 

   /\ G.Unit ::=   G.VariableDeclaration 
                 | G.ConstantDeclaration 
                 | G.Assumption 
                 | AtLeast4("-") 

   /\ G.VariableDeclaration ::=  
          Tok({"VARIABLE", "VARIABLES"}) & CommaList(Identifier)

   /\ G.ConstantDeclaration ::=   
           Tok({"CONSTANT", "CONSTANTS"}) & CommaList(G.OpDecl)



   /\  G.OperatorDefinition ::=  (   Identifier
                                & tok("==") 
                                & G.Expression  
G.FiniteSet ::= tok("{") &  CommaList(G.Identifier)  &  tok("}") 

/\ G.Expression  ::= 
            G.GeneralIdentifier 

         |  G.GeneralIdentifier & tok("(") 
              & CommaList(G.Argument) &  tok(")") 

         |  G.GeneralPrefixOp & G.Expression 

         |  G.Expression & G.GeneralInfixOp & G.Expression 

         |  G.Expression & G.GeneralPostfixOp

         |  tok("(") & G.Expression & tok(")") 

         
         

        |  G.Identifier & tok("[") & CommaList(Identifier) & tok("]")
  

       |  tok("[") & CommaList(Identifier & tok("|->") & G.Identifier)    
               & tok("]") 



       |      tok("[") 
           &  G.Expression 
           &  tok("EXCEPT") 
           &  CommaList(  tok("!")
                           | tok("[") & CommaList(G.Identifier) & tok("]"))^+ 
                        & tok("=") & G.Expression ) 
           &  tok("]") 

      |  tok("<<") & CommaList(G.Identifier) & tok(">>")  (* I am not sure how to define multi-dim sequance *)
      

       |  tok("IF") & G.Expression & tok("THEN")  
              & G.Expression & tok("ELSE") & G.Expression 

      

      |  tok("CASE") 
         &  ( LET CaseArm == 
                     G.Expression & tok("->") & G.Expression
              IN  CaseArm & (tok("[]") & CaseArm)^* )
         &  (    Nil 
              |  (tok("[]") & tok("OTHER") & tok("->") & G.Expression)) 


     |  (tok("/\\") & G.Expression)^+ 

     |  (tok("\\/") & G.Expression)^+ 

     |  Number 

     |  String 



  
 /\ G.Quantified_Expression  ::= |  Tok({"\\A", "\\E"}) &  CommaList(Identifier) 
                & tok(":") & G.Expression  
                
                 
  

   /\ G.Argument ::=   G.Expression  
                     | G.GeneralPrefixOp 
                     | G.GeneralInfixOp  
                     | G.GeneralPostfixOp  

   /\ G.InstancePrefix ::=  
        (    Identifier 
          &  (   Nil 
               | tok("(") & CommaList(G.Expression) & tok(")") ) )^*  

   /\ G.GeneralIdentifier ::= G.InstancePrefix & Identifier 
   /\ G.GeneralPrefixOp   ::= G.InstancePrefix & PrefixOp 
   /\ G.GeneralInfixOp    ::= G.InstancePrefix & InfixOp
   /\ G.GeneralPostfixOp  ::= G.InstancePrefix & PostfixOp  

   /\ G.ModuleDefinition ::=   G.NonFixLHS & tok("==") & G.Instance  





   

IN LeastGrammar(P)

=============================================================================
Leslie Lamport, Specifying systems: The tla+ language and tools for hardware and software
 engineers, Addison- Wesley, 2002.
=============================================================================

