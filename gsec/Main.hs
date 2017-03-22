{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Parser
import CmdLineParser
import Repl

import System.Process hiding (env)
import System.Environment
import System.Exit
import qualified Data.Text.IO as T

main :: IO ()
main = do
    (args, files) <- getArgs >>= parse 
    if Static `elem` args 
       then do
           _ <- createProcess $ proc "cat" ["./logo"]
           contents <- T.readFile $ head files
           let output = parseProgram (head files) contents
           case output of
                Left err -> print err
                Right output' -> putStrLn $ "Principle types after static typechecking:\n" ++ show output'
       else if Dynamic `elem` args
               then repl
               else do
                   putStrLn "no option chosen"
                   exitSuccess
