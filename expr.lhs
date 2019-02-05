> module ArithEval where
> import Term

Homework 3 Derick Falk




isNumerical

> isNumerical :: Term t -> Bool
> isNumerical t = case t of 
>                     Z       -> True
>                     Succ(_) -> True
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


Test suite

> main = do 
>          putStrLn $ "The result of first test suite"  

answers in order: Z
                  Z
                  F
                  T
                  If IsZ(Z) T F
                  Succ(Succ(Succ(Prd(Z))))
                  Succ(Succ(Succ(Z)))                  

>          putStrLn $ "(multi step) eval (IsZ Z): " ++ show (eval (IsZ Z))

>          putStrLn $ "(one step) eval1 (IsZ Z): " ++ show (eval1 (IsZ Z))

>          putStrLn $ "(multi step) eval (If T F T): " ++ show (eval (If T F T))

>          putStrLn $ "(multi step) eval (If (IsZ(Prd(Z))) T F): " ++ show (eval (If (IsZ(Prd(Z))) T F))

>          putStrLn $ "(one step) eval1 (If (IsZ(Prd(Z))) T F): " ++ show (eval1 (If (IsZ(Prd(Z))) T F))

>          putStrLn $ "(one step) eval1 (Succ(Succ(Succ(Prd(Prd(Succ(Z))))))): " ++ show (eval1 (Succ(Succ(Succ(Prd(Prd(Succ(Z))))))))
>          putStrLn $ "eval (big step) eval (Succ(Succ(Succ(Prd(Prd(Succ(Z))))))): " ++ show (eval (Succ(Succ(Succ(Prd(Prd(Succ(Z))))))))
