{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Parser
import Eval
import Pretty
import Syntax
import qualified Data.Text as T
import qualified Data.Text.IO as T

hoistError :: Either Error (Term, Type) -> T.Text
hoistError ex = 
    case ex of
         Left err -> T.pack $ "\ESC[1;31mError: \ESC[0m" ++ err
         Right (x,y) -> T.pack $ ppterm x ++ ":" ++ pptype y

main :: IO ()
main = do
    (file:_) <- getArgs
    contents' <- readFile file
    let program = parseProgram "<from file>" contents' 
    case program of
         Left err -> print err
         Right ast -> mapM_ (T.putStrLn . hoistError . typeAndEval) ast
