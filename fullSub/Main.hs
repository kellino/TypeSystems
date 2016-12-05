module Main where

import Syntax
import Parser
import TypeCheck
import Environment
import PrettyPrint
import Eval

import Text.PrettyPrint.ANSI.Leijen
import System.Environment (getArgs)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Exit
import System.Directory (doesFileExist)

main :: IO ()
main = do
    args <- getArgs
    if null args
       then T.putStrLn (T.pack "File error") >> exitFailure
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
                           let ctx' = addToContext $ filterAscrips r
                           let output = zip (T.lines contents) r
                           mapM_ (process ctx') output
                           exitSuccess
              else T.putStrLn (T.pack "Error") >> exitFailure

process :: Show a => TypeEnv -> (T.Text, Either a Term) -> IO ()
process _ (x, Left err) = T.putStr $ remComments x `T.append` T.pack (show err)
process nctx (x, Right r) = do
    let ty = runTypeOf initEnv nctx r
    let e = runEval r
    T.putStr $ remComments x `T.append` T.pack " "
    case e of
         Left err -> T.putStr (T.pack err)
         Right ev -> T.putStr (T.pack $ show ev)
    putDoc $ dullmagenta (text " ⇒ ") <> disp ty <> hardline
