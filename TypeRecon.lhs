> module TypeRecon where
> import Terms
> import Control.Applicative

typesubst

> maybeConcat (Just a) (Just b) = Just (a ++ b) 

> typeSubst ::[(Type t, Type t)]  -> (Type t) -> (Type t)
> typeSubst [] t = t
> typeSubst f@(((A num),y):xs) ty = case ty of
>                                   Bool        -> Bool
>                                   Nat         -> Nat
>                                   a `Arrow` b -> typeSubst xs ((typeSubst f  a) `Arrow` (typeSubst f b)) 
>                                   a `Cross` b -> typeSubst xs ((typeSubst f a)  `Cross` (typeSubst f b)) 
>                                   (A b)       -> if num == b then y else ty
>                                  


> typeIn :: Type t -> Type t -> Bool
> typeIn a b = case b of
>              Bool           -> a == Bool
>              Nat            -> a == Nat
>              a' `Arrow` b'  -> (typeIn a a') || (typeIn a b')
>              a' `Cross` b'  -> (typeIn a a') || (typeIn a b')
>              ListType a'    -> (typeIn a a') 
>              f              -> if (a==f) then True else False 



 mgu :: Type t -> Type t ->

> mgu :: (Type t, Type t) -> Maybe [(Type t, Type t)]
> mgu (a,b) = case (a,b) of
>           (Bool,Bool)                          -> Just [] 
>           (Nat,Nat)                            -> Just []
>           (Unit,b')                            -> Just []
>           (a',Unit)                            -> Just []
>           ((a' `Arrow` b'),(a'' `Arrow` b''))  -> let t   = if Nothing  /= mgu (a',a'') then mgu (a',a'') else Nothing
>                                                       t' = if Nothing /= mgu (b',b'') then mgu (b',b'') else Nothing  
>                                                   in if t == Nothing || t' == Nothing then Nothing else maybeConcat t t'
>           ((a' `Cross` b'),(a'' `Cross` b''))  -> let t = if Nothing  /= mgu (a',a'') then mgu (a',a'') else Nothing
>                                                       t' = if Nothing /= mgu (b',b'') then mgu (b',b'') else Nothing
>                                                   in if t == Nothing || t' == Nothing then Nothing else maybeConcat t t' 
>           (a'@(A _),b')                        -> if (not (typeIn a' b')) 
>                                                   then Just [(a',b')]
>                                                   else (if (a' /= b') then Nothing else Just []) --Fail or Id                                       
>           (a',b'@(A _))                        -> if (not (typeIn b' a'))
>                                                   then Just [(a',b')]
>                                                   else (if (a' /= b') then Nothing else Just [])
>           d                                    -> Nothing   

> typeVars [] = []
> typeVars ((_,A name):xs) = (A name:(typeVars xs))
> typeVars (_:xs)          = typeVars xs

 
> freshVar var@(A name) = (A (name ++ "'") ) 

> getBind var [] = Nothing
> getBind var ((x,binding):xs) = if var == x then Just binding else getBind var xs  

> typeFromContext t ctx = case t of
>                         t'@(Var n)     -> if getBind t' ctx == Nothing then Error else let Just t'' = getBind t' ctx in t'' 

> addBinding ctx ty x = ((x,ty):ctx)  

TODO typeConstraint

> typeCon t@(Var name) uvar ctx      = (ty, uvar, [])
>                                      where ty = typeFromContext t ctx

> typeCon t@(Lam name term) uvar ctx = let ctx' = addBinding ctx uvar (Var name)
>                                          (ty2,fresh,constr) = typeCon term uvar ctx' 
>                                      in ((Arrow uvar ty2), fresh, constr)                        

> typeCon t@(App t' t'') uvar ctx    = let (ty1,var,constr1)  = typeCon t' fresh1 ctx  
>                                          (ty2,var2,constr2) = typeCon t'' fresh2 ctx  
>                                      in (fresh3,(freshVar fresh3),([(ty1, Arrow ty2 fresh3)] ++ constr1 ++ constr2))
>                                      where fresh1    = freshVar uvar
>                                            fresh2    = freshVar fresh1
>                                            fresh3    = freshVar fresh2
> typeCon Z uvar ctx                 = (Nat,uvar,[])
> typeCon (Succ(t)) uvar ctx         = let (ty1, nextuvar, constr1) = typeCon t uvar ctx
>                                      in (Nat, nextuvar, ([(ty1,Nat)] ++ constr1))
> typeCon (Prd(t))  uvar ctx         = let (ty1, nextuvar, constr1) = typeCon t uvar ctx
>                                      in (Nat, nextuvar, [(ty1,Nat)] ++ constr1)
> typeCon (IsZ(t))  uvar ctx         = let (ty1, nextuvar, constr1) = typeCon t uvar ctx
>                                      in (Bool, nextuvar, [(ty1,Nat)] ++ constr1)
> typeCon (T)       uvar ctx         = (Bool,uvar,[])
> typeCon (F)       uvar ctx         = (Bool,uvar,[]) 
> typeCon (If t t' t'') uvar ctx     = let (ty1,nextuvar,constr1)  = typeCon t uvar ctx 
>                                          (ty2,nextuvar2,constr2) = typeCon t' nextuvar ctx
>                                          (ty3,nextuvar3,constr3) = typeCon t'' nextuvar2 ctx
>                                          newconstr = [(ty1,Bool)] ++ [(ty2, ty3)]
>                                      in (ty3, nextuvar3, newconstr)

typeCheck runs typeCon and then mgu on the result using wrapper function

 typeCheck :: Term t -> Bool

> isError (Just _) = False
> isError Nothing = True

> typeCheck term    = let (_,_,constr) = (typeCon term (A "A") [])
>                     in (typeCheck' constr)                  

> typeCheck' []     = Just []
> typeCheck' (x:xs) = if mgu x == Nothing then Nothing else typeCheck' xs 





