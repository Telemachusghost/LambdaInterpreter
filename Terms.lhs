Combined Lambda and Arith terms

> module Terms where
> import GHC.Generics (Generic)
> import Control.Applicative

 import Generic.Random --Does not work on ada

> type Name = [Char]

> data Term t = T | F | Z | If (Term t) (Term t) (Term t) | IsZ (Term t) | Prd (Term t) 
>               | Succ (Term t) | Var Name | Lam Name (Type t) (Term t) | App (Term t) (Term t)
>               | Let Name (Term t) (Term t) | UnitTerm | Seq (Term t) (Term t)  
>               | Pair (Term t) (Term t) | Pair1 (Term t) (Term t) | Pair2 (Term t) (Term t)  
>               | EmptyList | List (Term t) (Term t) | Head (Term t) | Tail (Term t) 
>               | IsNil (Term t) | Fix (Term t)   deriving (Show, Eq, Read)
                

> data Type t = Bool | Nat | Arrow (Type t) (Type t)  | NumericalError | GuardError | AppError | ConditionalError | EmptyContextError  | Error  
>               | SequenceError | A Name | Unit | (Type t) `Cross` (Type t) | ListType (Type t)   deriving (Show, Eq, Read)



 instance Arbitrary a => Arbitrary (Term t) where 
     arbitrary =  genericArbitraryRec uniform `withBaseCase` return T

 instance Arbitrary a => Arbitrary (Term t) where
     arbitrary = sized $ \n -> 
                             if n == 0 then
                                 return Z
                             else
                                 oneof
                                 [ return Z 
                                  , resize (n `div` 3) $
                                        Lam <$> arbitrary <*> arbitrary <*> arbitrary      


I could still work on the testing side a lot 

 instance Arbitrary (Term t) where
     arbitrary = elements 
                 [
                  Succ(Z),
                  Succ(Z),
                  Prd(Z),
                  IsZ(Z),
                  Succ(Succ(Succ(Z))) 
                 ]
