> import Eval
> import Terms
> import Lambda

 import TypeChecker

> import TypeRecon

import Test.QuickCheck

Evaluation Testing

 idTest term = (eval (App (id') (term))) == term
 renameTest t = (rename t "y" "z") == (rename t "y" "z") 
 kTest term = (eval (App (k) (term))) == Var "y"

 succProp term = eval (Succ(term)) /= Succ(term)
 succTest = quickCheck succProp

Type Checking Testing

 tcheckAppProp term | typeOf [] term == Nat =  (typeOf [("x",Nat)] (App (Lam "x" (Var "x")) (term))) == Nat
                    | typeOf [] term == Bool =  (typeOf [("x",Bool)] (App (Lam "x" (Var "x")) (term))) == Bool  

 tcheckAppTest = quickCheck tcheckAppProp



> typet1 = typeCheck ((Lam "x" (Var "x")) ) -- Arrow A A            -- A -> A
> typet2 = typeCheck (Lam "f" (Lam "x" (App (Var "x") (Var "f")) )) -- A -> ((A -> B) -> B)
> typet3 = typeCheck (Lam "f" (Lam "x" (Lam "d" (App (Var "f") (App (Var "x")(Var "d")))))) -- This one is hard to decipher from output but its (A->B)->(C->A)->(C->B)
> typet4 = typeCheck (Fix (Lam "x" (Var "x"))) -- C

> typet5 = typeCheck (App (Fix (Lam "x" (If (IsZ(Var "x")) T F))) (T)) -- This should not type
> typet6 = typeCheck (Lam "x" (If (IsZ(Var "x")) T F)) -- Nat -> Bool
> typet7 = typeCheck (App (Lam "x" (If (IsZ(Var "x")) T F)) T) -- Should not type
> typet8 = typeCheck (App (Lam "x" (If (IsZ(Var "x")) T F)) Z) -- This should be Bool but I think my app is a little messed up because it comes up as just a freshvar
                                                                 
                                                                  I fixed this it was an issue with typesubst and my typechecker

> typet9 = typeCheck (App (Fix (Lam "f" (Lam "x" (If (IsZ(Var "x")) T F)))) (Z)) -- This should type

> typeEvenOdd  = typeCheck (App (Fix (Lam "f" (Lam "x" (If (IsZ(Var "x")) T (If (IsZ(Prd(Var "x"))) F (App (Var "f") (Prd(Prd(Var "x")))))) ) )) Z) --Works Now!!!
> typeEvenOdd2 = typeCheck (App (Fix (Lam "f" (Lam "x" (If (IsZ(Var "x")) T (If (IsZ(Prd(Var "x"))) F (App (Var "f") (Prd(Prd(Var "x")))))) ) )) T) --Doesnt work now!!!

                                    