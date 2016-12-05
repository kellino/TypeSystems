module Main where

import Parser
import TypeCheck
import Syntax
import Environment
import PrettyPrint

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
    (file:_) <- getArgs
    contents <- T.readFile file
    let res = parseProgram file contents
    case res of
         Left err -> print err
         Right r -> do
             let ctx' = addToContext $ filterAscrips r
             let output = zip (T.lines contents) r 
             mapM_ (display ctx') output

display :: Show a => TypeEnv -> (T.Text, Either a Term) -> IO ()
display _ (x, Left err) = T.putStrLn $ remComments x `T.append` T.pack " ⇒ " `T.append` T.pack (show err)
display nctx (x, Right r) = T.putStrLn $ remComments x `T.append` T.pack " ⇒ " `T.append` T.pack (show (runTypeOf initEnv nctx r))
