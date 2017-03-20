module Main where

import Syntax
import Parser
import Static
import CmdLineParser
import Lattice

import System.Process hiding (env)
import System.Environment
import System.Exit
import qualified Data.Text.IO as T
import Control.Monad.State
import Data.Monoid ((<>))

main :: IO ()
main = do
    (args, files) <- getArgs >>= parse 
    if TypeCheck `elem` args 
       then do
           _ <- createProcess $ proc "cat" ["./logo"]
           contents <- T.readFile $ head files
           let output = parseProgram (head files) contents
           case output of
                Left err -> print err
                Right output' -> do
                    putStrLn "Principle types after static typechecking:\n"
                    evalStateT (mapM_ (execute . hoistError) output') emptyContext
                    --mapM_ process output'
       else if Repl `elem` args
               then putStrLn "not written yet"
               else exitFailure

type StTyOut a = StateT Ctx IO a

execute :: Term -> StTyOut ()
execute t = do
    st <- get
    let mod' = runStatic st t
    case mod' of
         Left err -> liftIO $ print err
         Right (_, m) -> do
            let st' = st { gammactx = gammactx m <> gammactx st }
            put st'
            liftIO $ print mod'

hoistError :: Show t => Either t Binding -> Term
hoistError (Left err) = error $ show err
hoistError (Right (_, a)) = a

{-process :: Show t => Either t (String, Term) -> IO ()-}
{-process (Left err) = print err-}
{-process (Right (n, a)) = print n-}
