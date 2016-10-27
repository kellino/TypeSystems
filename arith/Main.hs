module Main where

import Parser
import Arith

main :: IO ()
main = do
    file <- getLine
    contents' <- readFile file
    let code = parseProgram "<from file>" contents'
    case code of
         Left err -> print err
         Right res -> mapM_ run res
