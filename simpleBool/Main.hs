module Main where

import Parser 
import Pretty 
import Eval
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Text.PrettyPrint.ANSI.Leijen

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
               then process file
               else do 
                  putDoc $ red (text "Error") <> text ": file " <> dullgreen (text file) <> text " does not exist\n"
                  exitFailure

process :: FilePath -> IO ()
process file = do
    prog <- readFile file
    let res = parseProgram file $ lines prog
    case res of
         Left err -> print err
         Right r -> do
             let res' = mapM runEval r
             case res' of
                  Left err -> print err
                  Right r' -> mapM_ (\(x, y) -> putDoc $ text (pp x) <> red (text " ⇒ ") <> ppr 0 y <> hardline) (zip (lines prog) r')

pp :: String -> String
pp = map (\x -> if x == '\\' then 'λ' else x)
