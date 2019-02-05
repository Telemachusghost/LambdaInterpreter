> module TypeRecon where
> import Terms


typesubst

> typeSubst ::[(Type t, Type t)]  -> (Type t) -> (Type t)
> typeSubst [] t = t
> typeSubst f@(((A num),y):xs) ty = case ty of
>                                   Bool        -> Bool
>                                   Nat         -> Nat
>                                   a `Arrow` b -> typeSubst xs ((typeSubst f  a) `Arrow` (typeSubst f b)) 
>                                   a `Cross` b -> typeSubst xs ((typeSubst f a)  `Cross` (typeSubst f b)) 
>                                   (A b)       -> if num == b then y else ty
>                                  

TODO typeIn

> typeIn :: Type t -> Type t -> Bool
> typeIn a b = case b of
>              Bool           -> a == Bool
>              Nat            -> a == Nat
>              a' `Arrow` b'  -> (typeIn a a') || (typeIn a b')
>              a' `Cross` b'  -> (typeIn a a') || (typeIn a b')
>              ListType a'    -> (typeIn a a') 
>              f              -> if (a==f) then True else False 


TODO mgu

 mgu :: Type t -> Type t ->

> mgu :: (Type t, Type t) -> Maybe [(Type t, Type t)]
> mgu (a,b) = case (a,b) of
>           (Bool,Bool)                          -> Just [] 
>           (Nat,Nat)                            -> Just []
>           (Unit,b')                            -> Just []
>           (a',Unit)                            -> Just []
>           ((a' `Arrow` b'),(a'' `Arrow` b''))  -> let t   = if Just t  == mgu (a',a'') then t else Nothing
>                                                       t'' = if Just t' == mgu (b',b'') then t' else Nothing  
>                                                   in if t == Nothing || t'' Nothing then Nothing else Just (t ++ t')
>           ((a' `Cross` b'),(a'' `Cross` b''))  -> let Just t  = mgu (a',a'')
>                                                       Just t' = mgu (b',b'')
>                                                   in  Just (t ++ t') 
>           (a'@(A _),b')                        -> if (not (typeIn a' b')) 
>                                                   then Just [(a',b')]
>                                                   else (if (a' /= b') then Nothing else Just []) --Fail or Id                                       
>           (a',b'@(A _))                        -> if (not (typeIn b' a'))
>                                                   then Just [(a',b')]
>                                                   else (if (a' /= b') then Nothing else Just [])
>           d                                    -> Nothing   

TODO unify
