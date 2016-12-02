module Main where

import Syntax
import Parser
import Eval
import PrettyPrint

import System.Environment (getArgs)
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text.IO as T

main :: IO ()
main = do
    (file:_) <- getArgs
    contents <- T.readFile file
    let res = parseProgram file contents
    case res of 
         Left err -> print err
         Right res' -> mapM_ cleverPrint res'

cleverPrint :: Show a => Either a Term -> IO ()
cleverPrint (Left err) = print err
cleverPrint (Right r) = putDoc . display $ typeAndEval r
