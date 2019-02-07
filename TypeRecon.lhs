> module TypeRecon where
> import Terms
> import Control.Applicative

Recursive typing is not working properly and have not tested typing on let or pair much if at all.

Typing seems to be working correctly for simple things like applications, if statements, etc, but I would really like Fixed/recursion to work properly

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

 tySubs [(a,b):xs]

> tyId (Arrow t t')       = t'
> tyId (a `Cross` b)      = (a `Cross` b)
> tyId (a)                = a


> typeCon t@(Var name) uvar ctx      = (ty, uvar, [])
>                                      where ty = typeFromContext t ctx

> typeCon t@(Lam name term) uvar ctx = let ctx' = addBinding ctx uvar (Var name)
>                                          (ty2,fresh,constr) = typeCon term uvar ctx' 
>                                      in ((Arrow uvar ty2), fresh, constr)                        

I think there is something wrong with app case because it breaks many different things

> typeCon t@(App t' t'') uvar ctx    = let (ty1,nextuvar,constr1)  = typeCon t' uvar ctx  
>                                          (ty2, nextuvar2,constr2) = typeCon t'' (freshVar nextuvar) ctx
>                                          fresh                   = freshVar nextuvar2
>                                          tyid                    = tyId ty1
>                                      in (fresh, fresh,([(ty1, Arrow ty2 fresh )] ++ constr1 ++ constr2))

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
>                                      in (ty3, nextuvar3, newconstr ++ constr1 ++ constr2 ++ constr3)
> typeCon (Fix t) uvar ctx           = let (ty1, nextuvar, constr1) = typeCon t uvar ctx
>                                          fresh = freshVar nextuvar
>                                      in  (fresh, fresh, [(ty1, Arrow fresh fresh )] ++ constr1)
> typeCon (Pair t t') uvar ctx       = let (ty1, nextuvar, constr1)  = typeCon t uvar ctx
>                                          (ty2, nextuvar1, constr2) = typeCon t' nextuvar ctx 
>                                           in (ty1 `Cross` ty2, nextuvar1, constr1 ++ constr2)
> typeCon (Pair1 t _) uvar ctx       = let (ty1, nextuvar, constr) = typeCon t uvar ctx
>                                          in (ty1, nextuvar, constr)
> typeCon (Pair2 _ t) uvar ctx       = let (ty1, nextuvar, constr) = typeCon t uvar ctx
>                                           in (ty1, nextuvar, constr)
> typeCon (Let term eqTerm inTerm) uvar ctx   = let (ty1, nextuvar, constr)   = typeCon eqTerm uvar ctx
>                                                   (ty2, nextuvar2, constr1) = typeCon inTerm nextuvar ctx
>                                                   in (Arrow ty1 ty2, nextuvar2, constr1 ++ constr)

typeCheck runs typeCon and then mgu on the result using wrapper function

 typeCheck :: Term t -> Bool

> isError (Just _) = False
> isError Nothing = True

> typeCtx _ [] = []
> typeCtx t@(A name) ((A name1, var):xs) = if name == name1 then var else typeCtx t  xs

> addCtx t@(A name, var) ctx = (t:ctx)
> addCtx t@(var,A name) ctx  = ((A name,var):ctx)  

> typeCheck term    = let (_,_,constr) = (typeCon term (A "A") [])
>                     in (typeCheck' constr []) 

> typeCheck''' term    = let (_,_,constr) = (typeCon term (A "A") [])
>                        in (typeCheck'' constr)                 

> typeCheck' [] _     = Just []
> typeCheck' ((a,b):xs) ctx = let t  = typeSubst ctx a   
>                                 t' = typeSubst ctx b
>                                 
>                             in if mgu (t,t') == Nothing 
>                                then Nothing 
>                                else if mgu (t,t') == Just [] 
>                                     then typeCheck' xs ctx
>                                     else let Just mg = mgu (t,t') in typeCheck' xs (addCtx (head mg) ctx)

> typeCheck'' [] = Just []
> typeCheck'' (x:xs) = if mgu x == Nothing then Nothing else typeCheck'' xs                               





