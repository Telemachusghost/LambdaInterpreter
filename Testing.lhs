> import Eval
> import Terms
> import Lambda
> import TypeChecker

import Test.QuickCheck

Evaluation Testing

> idTest term = (eval (App (id') (term))) == term
> renameTest t = (rename t "y" "z") == (rename t "y" "z") 
> kTest term = (eval (App (k) (term))) == Var "y"

 succProp term = eval (Succ(term)) /= Succ(term)
 succTest = quickCheck succProp

Type Checking Testing

> tcheckAppProp term | typeOf [] term == Nat =  (typeOf [("x",Nat)] (App (Lam "x" (Var "x")) (term))) == Nat
>                    | typeOf [] term == Bool =  (typeOf [("x",Bool)] (App (Lam "x" (Var "x")) (term))) == Bool  

> tcheckAppTest = quickCheck tcheckAppProp


