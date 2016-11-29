module Main where

import Parser
import Eval
import PrettyPrint
import Syntax (Term)

import System.Environment (getArgs)
import Text.PrettyPrint.ANSI.Leijen 
import System.Exit
import System.Directory (doesFileExist)
import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    if null args
       then putDoc (red (display "File error") <> display ": no file entered\n") >> exitFailure
       else do
           let file = head args
           exists <- doesFileExist file
           if exists 
              then do
                  contents <- T.readFile file
                  let res = parseProgram file contents
                  case res of 
                       Left err -> print err
                       Right r -> do
                           mapM_ dispAll $ zip (T.lines contents) r
                           exitSuccess
              else putDoc (red (display "File error") <> display ": the file " <> bold (display file) <> display " does not exist\n") >> exitFailure

dispAll :: (T.Text, Either t Term) -> IO ()
dispAll (x, Left _) = T.putStrLn $ T.pack "parse error in " `T.append` x
dispAll (x, Right y) = do
    T.putStr $ T.strip $ T.takeWhile (/= '#') x
    putDoc $ dullmagenta (text " ⇒ ") <> display (tyAndEval y)
