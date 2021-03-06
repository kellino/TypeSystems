{-# LANGUAGE OverloadedStrings #-}

module Main where


import Parser
import Eval
import Pretty
import TypeCheck
import SecTypes
import Syntax (Expr)

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
        Right res -> mapM_ (\(x, l) -> do
                          T.putStr $ stripComments l
                          putDoc $ red $ text " ⇒  "
                          putDoc $ evalExpr x <> hardline) $ zip res (T.lines prog)

evalExpr :: Show a => Either a Expr -> Doc
evalExpr p =
    case p of
         Left err -> (display . show) err -- major parse error
         Right p' ->
             case runSecTypeCheck p' of
                  Left _ -> text "error"
                  Right derivation -> 
                    case runTypeOf p' of
                         Left err -> display err
                         Right _ -> 
                            case runEval p' of 
                                 Left err -> display err
                                 Right res' -> display res' 
                                        <> hardline 
                                        <> blue (text "Derivation:")
                                        <> display derivation

stripComments :: T.Text -> T.Text
stripComments = T.strip . T.takeWhile (/= '#')
