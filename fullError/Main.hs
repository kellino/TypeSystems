module Main where

import Parser
import Eval
import TypeCheck

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Control.Arrow ((&&&))

main :: IO ()
main = do
    (file:_) <- getArgs
    contents <- T.readFile file
    let res = parseProgram file contents
    case res of
         Left err -> print err
         Right r -> mapM_ (print . (runEval &&& runTypeOf)) r
