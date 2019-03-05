
# General Info #

Lambda Interpreter 
Derick Falk
TESC Winter/Spring 2019

This is a simple lambda interpreter parsing is currently not supported, the abstract sytax is directly evaluated. In the file interpreter

The implementation takes inspiration from Pierce (ISBN:978-0262162098) and Sellinger (https://arxiv.org/pdf/0804.3434.pdf)

# BNF Abstract syntax #

Term ::= If Term Term Term | Succ Term | Prd Term |
         Z | T | F | Lam Name Name | Var Name | Pair Term Term
         | Pair1 Term Term | Pair2 Term Term | EmptyList | List Term Term
         | Head Term | Tail Term | IsNil Term | Fix Term | Type Type | TypeOf Term | Seq Term Term
         | UnitTerm | Let Name Term Term

Type ::= Nat | Bool | Arrow Type Type | Cross Type Type |  A Name | Unit | ListType Type

Name ::= string

# How to Get started #

Clone the repo then compile interpreter.lhs and try to figure it out. 

# A Few Things #

Fix Term is the fixed point combinator, the language allows recursion and as such is not normalizing and can have endless loops so beware.

List Term Term the first term is the head and the second term is either a list or the EmptyList term

IsNil Term is for lists 

TypeOf Term is just like haskells ghci ":t" command

Seq Term Term will create a sequence where Term1 needs to return a term of type Unit whereat Term2 is evaluated afterwards. (I am not sure of the use of this quite yet)

Let Name Term Term is the same as let name = Term1 in Term2 (haskell)


