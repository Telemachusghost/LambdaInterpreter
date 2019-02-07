> module Lambda where
> import qualified Data.Set as Set
> import Terms 
> import ArithEval (isNumerical)

For fv, bv, and vars I used the Set data pe because it does not allow duplicates.

function that produces a set of bound variables in Term t 

> bv ::  Term t -> Set.Set Name 
> bv t = case t of 
>        Lam x term   -> Set.union (Set.singleton x) (bv term)
>        App t1 t2    -> Set.union (bv t1) (bv t2)
>        Var t        -> Set.empty 

function that produces a set of free variables in Term t

> fv ::  Term t -> Set.Set Name 
> fv (Lam (var)  (lamterm)) = case lamterm of
>                            (App (lamterm1) (lamterm2)) -> Set.union (fv (Lam (var) lamterm1)) (fv (Lam (var)  lamterm2))
>                            Var var1                  -> if var /= var1 then Set.singleton var1 else Set.empty
>                            (Lam (var1) (term1))      -> Set.intersection (fv (Lam (var) (term1))) (fv (Lam (var1)  (term1)))
>                            If t t1 t2                -> Set.intersection (Set.intersection (fv (Lam (var) t)) (fv (Lam (var) t1))) (fv (Lam var t2))
>                            Fix t                     -> fv t
>                            _                         -> Set.empty
> fv lamterm = case lamterm of
>              (App (lamterm1) (lamterm2))               -> Set.union (fv (lamterm1)) (fv (lamterm2))
>              (Fix t)                                   -> fv t
>              Var v                                     -> Set.singleton v                
>              IsZ t                                     -> fv t
>              Prd t                                     -> fv t
>              Succ t                                    -> fv t
>              _                                         -> Set.empty

Produces the all variables by union of fv and bv

> vars ::  Term t -> Set.Set Name
> vars = (\x -> Set.union (bv x) (fv x))


rename function 

> rename ::  Term t -> Name -> Name ->  Term t
> rename lamterm y x = if Set.notMember y (fv $ lamterm) then rename' lamterm y x else lamterm

> rename' ::  Term t -> Name -> Name ->  Term t
> rename' lamterm y x = case lamterm of
>                       Var t                         -> if t == x then Var y else Var t 
>                       ( App (lamterm1) (lamterm2) ) -> App (rename' lamterm1 y x ) (rename' lamterm2 y x)  
>                       (Lam (t1)  (t2))               -> if x == t1 then Lam y  (rename' t2 y x) else Lam t1  (rename' t2 y x)

produces a fresh variable

> freshy ::  Term t -> Name -> Name 
> freshy lamterm y     | Set.notMember y (vars lamterm) = y
>                      | otherwise = freshy lamterm (y ++ "'")   

Need to change behaviour when variable that gets renamed is same as a variable we are substituting

> subst :: ( Term t, Name) ->  Term t ->  Term t
> subst (t1,x) term   = case term of
>                       Var x2              -> if x2 == x then t1 else term
>                       Succ t              -> if not(isNumerical t) then Succ(subst (t1,x) t) else Succ t
>                       Prd t               -> if not(isNumerical t) then Prd(subst (t1,x) t) else Prd t
>                       IsZ t               -> if not(isNumerical t) then IsZ(subst (t1,x) t) else IsZ t
>                       App term1 term2     -> App (subst (t1,x) term1)(subst (t1,x) term2) 
>                       Lam binder bound -> if x /= binder then 
>                                               (if (Set.notMember binder (fv t1)) then Lam binder  (subst (t1,x) bound) else subst (t1,x) newTerm) 
>                                           else subst (t1,x) bound
>                                           where fresh = freshy term binder
>                                                 newTerm = rename term fresh binder
>                       If t t' t''         -> (If (subst (t1,x) t) (subst (t1,x) t') (subst (t1,x) t''))
>                       t                   -> t

Alpha Equivalance I think this is a little bloated and could lose some weight 

> alphaEq ::  Term t ->  Term t -> Bool
> alphaEq (Var x) (Var y)                                  = (x == y)
> alphaEq (App t1 t2) (App t1' t2')                        = (t1 `alphaEq` t1') && (t2 `alphaEq` t2')   
> alphaEq t1@(Lam binder1  term1) t2@(Lam binder2 term2)    | binder1 == binder2 = term1 `alphaEq` term2
>                                                                 | otherwise = if (binder1 `Set.notMember` (vars term2)) 
>                                                                            then let nT2@(Lam nBind2 nT') = (rename t2 (binder1 ++ "'") binder2) 
>                                                                                     nT1@(Lam nBind1 nT)  = (rename t1 (binder1 ++ "'") binder1)
>                                                                                     in nT' `alphaEq` nT
>                                                                            else let nT2@(Lam nBind2 nT') = (rename t2 binder2 binder1)
>                                                                                  in term1 `alphaEq` nT'
> alphaEq _ _                                              = False   

Test suite for alpha equivalence

 id'  = (Lam "x" (Var "x"))
 id'' = (Lam "y" (Var "y"))  -- The result of these two should be true
 k    = (Lam "x" (Var ("y"))) -- This should not be alpha equivalent to id' or id''
 k'   = (Lam "y" (Var "x")) -- AlphaEq to k  

 omega' = (App (Lam "y" (App (Var "y")(Var "y") ))) (Lam "y" (App (Var "y")(Var "y")))   
 omega'' = (App (Lam "y" (App (Var "x")(Var "y") ))) (Lam "y" (App (Var "y")(Var "y"))) -- Should fail slightly diff than normal omega

subst (Var "x", "z") (Lam "x" (Var "z")) this causes an error in subst
