module Main where

import System.Environment (getArgs)
import Parser
import Pretty

main :: IO ()
main = do
    (file:_) <- getArgs
    source <- readFile file
    mapM_ (print . hoistError . runTerm) $ lines source
