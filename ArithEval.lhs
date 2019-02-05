> module ArithEval where
> import Terms

Homework 3 Derick Falk



isNumerical

> isNumerical :: Term t -> Bool
> isNumerical t = case t of 
>                     Z       -> True
>                     Succ(t) -> isNumerical t
>                     _       -> False

isVal 

> isVal :: Term t -> Bool
> isVal t = case t of 
>               T       -> True
>               F       -> True
>               t       -> isNumerical t              




> eval1 :: Term t -> Maybe (Term t)
> eval1 (If t1 t2 t3) | t1 == T               = Just t2
>                     | t1 == F               = Just t3
>                     | (eval1 t1) == Nothing = Nothing
>                     |  otherwise            = let Just t1' = eval1 t1 in (Just (If t1' t2 t3))
> eval1 (Succ t)  | t == Z = Nothing
>                 | (eval1 t) == Nothing = Nothing
>                 | otherwise = let Just t' = eval1 t in Just (Succ t')
> eval1 (Prd t) = case t of
>                     Z        -> Just Z
>                     (Succ t) -> if (isNumerical t) then Just t else Nothing
>                     t        -> let Just t' = eval1 t in (Just (Prd t')) 
> eval1 (IsZ t)   | t == Z = Just T
>                 | t == Succ(t) = if (isNumerical t) then Just F else Nothing
>                 | (eval1 t) == Nothing  = Nothing       
>                 | otherwise = let Just t' = eval1 t in Just (IsZ t') 
> eval1 _ = Nothing

Big step style

> eval t = let t1 = eval1 t in if t1 == Nothing then t else let Just t2 = t1 in eval t2 


