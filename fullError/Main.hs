module Main where

import Parser
import Eval
import PrettyPrint
import Syntax (Term)

import System.Environment (getArgs)
import Text.PrettyPrint.ANSI.Leijen (putDoc, (<>), text, dullmagenta)
import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
    (file:_) <- getArgs
    contents <- T.readFile file
    let res = parseProgram file contents
    case res of
         Left err -> print err
         Right r -> mapM_ dispAll $ zip (T.lines contents) r

dispAll :: (T.Text, Either t Term) -> IO ()
dispAll (x, Left _) = T.putStrLn $ T.pack "parse error in " `T.append` x
dispAll (x, Right y) = do
    T.putStr $ T.strip $ T.takeWhile (/= '#') x
    putDoc $ dullmagenta (text " ⇒ ") <> display (tyAndEval y)
