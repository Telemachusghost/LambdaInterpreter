> module Interpreter where
> import TypeChecker
> import Eval 
> import Terms
> import System.IO
> import Control.Exception
> import System.IO.Error
> import Text.Read

> data Control = END

> main :: IO ()
> main = do
>      line <- getLine
>      if null line
>          then return ()
>          else let code = reads line in 
>               if code == []
>               then do parseException
>                       main
>              else if typeChecking code then do   
>                       typeError
>                       main
>                   else do
>                   runShow code
>                   main
>           
>                                                             


                                                                 
> parseException =  putStrLn "Bad parse"

> typeError :: IO ()
> typeError = putStrLn "Poorly typed please check your code"




 
> runShow = putStrLn . show . eval . fst . head
> typeChecking = isError . typeOf [] . fst . head
                                              
