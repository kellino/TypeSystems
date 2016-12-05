module Main where

import Syntax
import Parser
import TypeCheck
import Environment
import PrettyPrint
import Text.PrettyPrint.ANSI.Leijen

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
             mapM_ (process ctx') output

process :: TypeEnv -> (T.Text, Either a Term) -> IO ()
process _ (x, Left err) = T.putStr (remComments x)
process nctx (x, Right r) = do
    T.putStr (remComments x)
    putDoc $ dullmagenta (text " ⇒ ") <> disp (runTypeOf initEnv nctx r) <> hardline
