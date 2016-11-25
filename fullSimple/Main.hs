module Main where

import Parser
import Environment
import Eval
import PrettyPrint

import System.Environment
import Text.PrettyPrint.ANSI.Leijen
import System.Exit
import System.Directory

main :: IO ()
main = do
    args <- getArgs
    if null args
       then do 
            putDoc $ red (text "Error") <> text ": you did not enter a filename\n"
            exitFailure
       else do 
            let file = head args
            found <- doesFileExist file
            if found 
               then do
                   process file
                   exitSuccess
               else do 
                  putDoc $ red (text "Error") <> text ": file " <> dullgreen (text file) <> text " does not exist\n"
                  exitFailure

process :: FilePath -> IO ()
process path = do
    contents <- readFile path
    let res = parseProgram contents (lines contents)
    case res of
         Left err -> print err
         Right r' -> mapM_ (\(x, y) -> putDoc $ display x <> red (text " ⇒ ") <> display (eval' y) <> hardline) (zip (lines contents) r')
    where eval' = typeAndEval emptyEnv
