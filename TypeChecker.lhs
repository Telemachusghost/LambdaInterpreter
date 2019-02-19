> module TypeChecker where
> import ArithEval
> import Test.QuickCheck
> import Lambda
> import Terms


This is an older typechecker that uses type ascription.


Checks if a type is an error type

> isError NumericalError   = True
> isError GuardError       = True
> isError ConditionalError = True
> isError AppError         = True
> isError Error            = True
> isError SequenceError    = True
> isError _                = False

Adds binding to context

> addBinding ctx x ty = ((x,ty):ctx)

Retreives a binding from a ctx

> getBinding ((x,ty):ctx) var = if var == x then (x,ty) else (getBinding ctx var)
> getBinding [] var           = (var,EmptyContextError)

Retreives the type from a particular context

> getTypeFromCtx ctx var = case (getBinding ctx var) of
>                          (x,ty)        -> if ty /= EmptyContextError then ty else EmptyContextError


typeOf function - Gives type of a function depending on ctx

> typeOf ctx t = case t of
>                    Var var         -> (getTypeFromCtx ctx var) 
>                    Lam var ty1 lamterm -> let ctx' = (addBinding ctx var ty1) in
>                                               let ty2 = typeOf ctx' lamterm in
>                                                   (Arrow (ty1) (ty2))
>                    App lam1 lam2   -> let ty1 = typeOf ctx lam1 
>                                           ty2 = typeOf ctx lam2 in
>                                               case ty1 of
>                                                   (Arrow ty11 ty12) -> if ty2 == ty11 then ty12 else AppError
>                                                   f                 -> AppError
>                    Z              -> Nat
>                    T              -> Bool
>                    F              -> Bool
>                    UnitTerm       -> Unit   
>                    Succ(t)        -> if (typeOf ctx t) == Nat then Nat else NumericalError 
>                    Prd(t)         -> if (typeOf ctx t) == Nat then Nat else NumericalError 
>                    IsZ(t)         -> if (typeOf ctx t) == Nat then Bool else NumericalError 
>                    If t1 t2 t3    -> let ty1 = typeOf ctx t1
>                                          ty2 = typeOf ctx t2
>                                          ty3 = typeOf ctx t3
>                                      in
>                                          if ty1 == Bool 
>                                          then 
>                                              if ty2 == ty3 then ty2 else GuardError
>                                          else 
>                                              ConditionalError
> 
>                    Seq t1 t2     -> if typeOf ctx t1 == Unit then typeOf ctx t2 else SequenceError
>                    Let sn st t   -> let ty1 = typeOf ctx st in typeOf ctx (App (Lam sn ty1 t) (st))    
>                    Pair t1 t2    -> let ty1 = typeOf ctx t1
>                                         ty2 = typeOf ctx t2
>                                         in
>                                         if isError ty1 || isError ty2 then Error else ty1 `Cross` ty2 
>                    Pair1 t1 _    -> typeOf ctx t1
>                    Pair2 _ t2    -> typeOf ctx t2
>                    EmptyList     -> ListType Unit
>                    List t1 t2    ->  if typeOf ctx t2 == ListType Unit then typeOf ctx t1 else if typeOf ctx t1 ==  typeOf ctx t2 then ListType (typeOf ctx t1) else Error
>                    Head (List t _) -> typeOf ctx t
>                    Tail (List _ t) -> typeOf ctx t
>                    IsNil _         -> Bool
>                    Fix (Lam _ (Arrow a a) _) -> a
>                    t             -> Error                     
