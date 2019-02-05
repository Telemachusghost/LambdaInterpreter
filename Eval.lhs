This combines lambda eval with arith eval 

> module Eval where
> import Terms


 import Lambda (subst, zero, one, add, omega, succ, test1, test2, test3, id', id'', k)

> import Lambda (subst)
> import ArithEval (isNumerical)
> import LambdaEval (isVal, isVar)

> eval1 :: Term t -> Maybe (Term t)

> eval1 (If t1 t2 t3) | t1 == T               = Just t2
>                     | t1 == F               = Just t3
>                     | (eval1 t1) == Nothing = Nothing
>                     |  otherwise            = let Just t1' = eval1 t1 in (Just (If t1' t2 t3))

> eval1 (Succ t)      | t == Z                  = Nothing
>                     | (eval1 t) == Nothing    = Nothing
>                     | otherwise = let Just t' = eval1 t in Just (Succ t')

> eval1 (Prd t) =     case t of
>                        Z       -> Just Z
>                        Succ t  -> if (isNumerical t) then Just t else Nothing
>                        t       -> let Just t' = eval1 t in (Just (Prd t'))

> eval1 (IsZ t)     | t == Z = Just T
>                   | t == Succ(t)          = if (isNumerical t) then Just F else Nothing
>                   | (eval1 t) == Nothing  = Nothing
>                   | otherwise             = let Just t' = eval1 t in Just (IsZ t')

> eval1 (App t1 t2) | not(isVal t1) && not(isVar t1) = let Just t1' = eval1 t1 in Just (App(t1')(t2))
>                   | not(isVal t2) && not(isVar t2) = if eval1 t2 == Nothing then 
>                                                                                 Just (App(t1)(t2)) 
>                                                                             else
>                                                                                 let Just t2' = eval1 t2 in  Just (App(t1)(t2'))
>                   | otherwise                      = let Lam x ty term = t1 in Just (subst (t2,x) term)                             --Need to add type checking to prevent cases such as (Succ Z)(_)
> eval1 (Let subName subTerm term) = let (Lam boundName ty _) = subTerm in if boundName == subName then Nothing else Just ( App (Lam subName ty term) (subTerm))
> eval1 (Seq UnitTerm t2) = Just t2
> eval1 (Seq t1 t2)       = let Just t1' = eval1 t1 in Just (Seq t1' t2)
> eval1 (Pair t1 t2)      | not(isVal t1) && not(isVar t1) = let Just t1' = eval1 t1 in Just (Pair t1' t2)
>                         | not(isVal t2) && not(isVar t2) = let Just t2' = eval1 t2 in Just (Pair t1 t2')
>                         | otherwise = Nothing
> eval1 (Pair1 t1 t2)     | not(isVal t1) && not(isVar t1) = let Just t1' = eval1 t1 in Just (Pair1 t1' t2)
>                         | otherwise = Just t1
> eval1 (Pair2 t1 t2)     | not(isVal t2) && not(isVar t2) = let Just t2' = eval1 t2 in Just (Pair2 t1 t2')
>                         | otherwise = Just t2
> eval1 (List t t1)       | not (isVal t) = let Just t' = eval1 t in  Just (List t' t1)
>                         | not (isVal t1) = let Just t1' = eval1 t1 in Just(List t t1')
>                         | otherwise      = Nothing
> eval1 (IsNil t)         | t == EmptyList = Just T
>                         | not (isVal t)  = eval1 t
>                         | otherwise      = Just F
> eval1 (Head (List t t1))| not (isVal t)  = eval1 t
>                         | otherwise      = Just t
> eval1 (Tail (List t t1))| not (isVal t1) = eval1 t
>                         | otherwise      = Just t1
> eval1 tm@(Fix t) | isVal t = let (Lam n ty t1) = t in Just (subst (tm,n) t1) 
>                  | otherwise = let Just t' = eval1 t in Just (Fix t')   
>                                                 
> eval1 _ = Nothing



> eval :: Term t -> Term t
> eval t | (eval1 t) == Nothing = t
>        | otherwise = let Just t1 = (eval1 t) in eval t1  
