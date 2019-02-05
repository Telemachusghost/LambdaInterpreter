> module LambdaEval where
> import Lambda
> import Terms
> import ArithEval (isNumerical)  
> import qualified Data.Set as Set


tests for eval1 Success or another variable show sucess failure appearing signifies a problem result

 idEval =  App id' ( Var "success" ) 
 kEval  =  App k (Var "Failure") 
 omegaEvalNT = App omega id'                              -- This never terminates 
 omegaEvalT  = App k omega                                -- I thought this would terminate but it doesn't because of CBV
 multistepEval = App (App id' id' )(id')                  -- Shows behaviour of multi step vs single step
 captureProbEval = App (Lam "z" (Lam "x" (Var "z") )) (Var "x")

Just a couple of utility functions

> isVal :: Term t -> Bool
> isVal t = case t of
>           Lam _ _ _ -> True
>           Succ t    -> isNumerical(t)
>           Z         -> True
>           T         -> True
>           F         -> True 
>           EmptyList -> True
>           List t t1 -> (isVal t) && (isVal t1)
>           Pair t t1 -> (isVal t) && (isVal t1)
>           t         -> False

> isVar :: Term t -> Bool
> isVar t = case t of
>           Var _ -> True
>           t     -> False  

The one step evaluator

> eval1 :: Term t -> Maybe (Term t)
> eval1 (App t1 t2) | not(isVal t1) && not(isVar t1) = let Just t1' = eval1 t1 in Just (App(t1')(t2))
>                   | not(isVal t2) && not(isVar t2) = let Just t2' = eval1 t2 in  Just (App(t1)(t2')) 
>                   | otherwise                      = let Lam x ty term = t1 in Just (subst (t2,x) term)
> eval1 _ = Nothing

multi step evaluator

> eval :: Term t -> Term t
> eval t | (eval1 t) == Nothing = t
>        | otherwise = let Just t1 = eval1 t in eval t1 
