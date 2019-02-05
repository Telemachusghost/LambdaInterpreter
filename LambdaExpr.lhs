> module LambdaExpr where
> import qualified Data.Set as Set

lambda data type


> type Name = String
> data LamTerm = Var Name | App LamTerm LamTerm | Lam Name LamTerm deriving (Show, Eq)

Example expressions

(p.56) App  

> pg56 = (App (Lam "x" (Var "x") ) (App (Lam "x" (Var "x")) ( Lam "z" (App( Lam "x" (Var "x") ) (Var "z") )) ))

> e = ( Lam "z" (App( Lam "x" (Var "x") ) (Var "z")))

(omega) no free variables and x is bound variable

> omega = App (Lam "x" (App (Var "x") (Var "x") ) )  (Lam "x" (App (Var "x") (Var "x") ) )

> test1 = (App (Var "x") (Var "y")) -- no bv [x,y] fv
> test2 = (App (Lam "x" (Var "x")) (Var "x")) -- x bv and x fv
> test3 = (Lam "z"(App (Var "z") (App (Var "x")(Var "y")))) -- used to test whether subst behaves properly if given input (Var "y") 

For fv, bv, and vars I used the Set data type because it does not allow duplicates.

function that produces a set of bound variables in LamTerm 

> bv :: LamTerm -> Set.Set Name
> bv (Lam (var) (lamterm)) = case lamterm of 
>                            (App (lamterm1) (lamterm2)) -> Set.union (bv (Lam (var) lamterm1)) (bv (Lam (var) lamterm2))
>                            Var var1                    -> if var == var1 then Set.singleton var1 else Set.empty
>                            (Lam (var1) (term1))        -> Set.union (bv (Lam (var) (term1))) (bv (Lam (var1) (term1)))
> bv lamterm = case lamterm of
>              (App (lamterm1) (lamterm2)) -> Set.union (bv (lamterm1)) (bv (lamterm2))
>              Var var1                    -> Set.empty


function that produces a set of free variables in LamTerm

> fv :: LamTerm -> Set.Set Name
> fv (Lam (var) (lamterm)) = case lamterm of
>                            (App (lamterm1) (lamterm2)) -> Set.union (fv (Lam (var) lamterm1)) (fv (Lam (var) lamterm2))
>                            Var var1                    -> if var /= var1 then Set.singleton var1 else Set.empty
>                            (Lam (var1) (term1))        -> Set.intersection (fv (Lam (var) (term1))) (fv (Lam (var1) (term1)))
> fv lamterm = case lamterm of
>              (App (lamterm1) (lamterm2))               -> Set.union (fv (lamterm1)) (fv (lamterm2))
>              Var v                                     -> Set.singleton v                

Produces the free variables by union of fv and bv

> vars :: LamTerm -> Set.Set Name
> vars = (\x -> Set.union (bv x) (fv x))


rename function 

> rename :: LamTerm -> Name -> Name -> LamTerm
> rename lamterm y x = if Set.notMember y (fv $ lamterm) then rename' lamterm y x else lamterm

> rename' :: LamTerm -> Name -> Name -> LamTerm
> rename' lamterm y x = case lamterm of
>                       Var t                         -> if t == x then Var y else Var t 
>                       ( App (lamterm1) (lamterm2) ) -> App (rename' lamterm1 y x ) (rename' lamterm2 y x)  
>                       (Lam (t1) (t2))               -> if x == t1 then Lam y (rename' t2 y x) else Lam t1 (rename' t2 y x)

produces a fresh variable

> freshy :: LamTerm -> Name -> Name
> freshy lamterm y     | Set.notMember y (vars lamterm) = y
>                      | otherwise = freshy lamterm (y ++ "'")   

Need to change behaviour when variable that gets renamed is same as a variable we are substituting

> subst :: (LamTerm, Name) -> LamTerm -> LamTerm
> subst (t1,x) term   = case term of
>                       Var x2           -> if x2 == x then t1 else term
>                       App term1 term2  -> App (subst (t1,x) term1)(subst (t1,x) term2) 
>                       Lam binder bound -> if x /= binder then 
>                                               (if (Set.notMember binder (fv t1)) then Lam binder (subst (t1,x) bound) else subst (t1,x) newTerm) 
>                                           else term
>                                           where fresh = freshy term binder
>                                                 newTerm = rename term fresh binder
