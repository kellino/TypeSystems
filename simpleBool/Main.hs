module Main where

import Parser (toplevel)
import Pretty 
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Text.Megaparsec (runParser)
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
               then do
                   prog <- readFile file
                   let res = runParser toplevel file prog
                   case res of
                        Left err -> print err
                        Right r -> mapM_ (\x -> putDoc $ ppr 0 x <> hardline) r
               else do 
                  putDoc $ red (text "Error") <> text ": file " <> dullgreen (text file) <> text " does not exist\n"
                  exitFailure
