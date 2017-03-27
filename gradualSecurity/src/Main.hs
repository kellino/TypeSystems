module Main where

import CmdLineParser
import Syntax
import Gamma
import Parser
import StaticCheck
import DynamicCheck
import Lattice
import PrettyPrint
import Processor
import Repl

import qualified Data.Text.IO as T
import System.Process
import System.Environment
import System.Exit
import Control.Monad.State.Strict (evalStateT)

main :: IO ()
main = do
    (args, files) <- getArgs >>= parse 
    if Static `elem` args 
       then do
           _ <- createProcess $ proc "cat" ["/home/david/Programming/Haskell/TypeSystems/gradSec/src/logo"]
           putStrLn "The results of static checking are: \n"
           contents <- T.readFile $ head files
           evalStateT (processStatic contents True) initCtx
       else if Dynamic `elem` args
               then repl 
               else do
                   putStrLn "no option chosen"
                   exitSuccess
