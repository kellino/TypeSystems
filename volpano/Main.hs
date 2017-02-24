{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Parser 
import TypeCheck
import SecTypes
--import Pretty 
import Eval
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
    prog <- T.readFile file
    let parsed = parseProgram file prog
    case parsed of
        Left err -> T.putStrLn (T.pack $ show err)
        Right res -> mapM_ (T.putStrLn . T.pack . evalExpr)  res

-- how can we rewrite this so it's easier to read?
-- consider using Control.Error
evalExpr :: Show a => Either a Expr -> String
evalExpr p =
    case p of
         Left err -> show err -- major parse error
         Right p' ->
             case runSecTypeCheck p' of
                  Left err -> err
                  Right _ -> 
                    case runTypeOf p' of
                         Left err -> err
                         Right _ -> 
                            case runEval p' of 
                                 Left err -> err
                                 Right res' -> show res'
