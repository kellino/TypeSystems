module Main where

import Parser
import Arith

import System.Environment

main :: IO ()
main = do
    (file:_) <- getArgs
    contents' <- readFile file
    let code = parseProgram "<from file>" contents'
    case code of
         Left err -> print err
         Right res -> mapM_ run res
