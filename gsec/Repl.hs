{-# LANGUAGE FlexibleContexts #-}

module Repl where

import Syntax
import Gamma
import Static
import Intrinsic
import Parser

import Data.List (foldl', isPrefixOf)
import Text.Megaparsec (ParseError, Dec)
import System.Console.Repline
import Control.Monad.State.Strict
import qualified Data.Text as T

type Repl a = HaskelineT (StateT Ctx IO) a

hoistError :: Show e => Either e a -> Repl a
hoistError (Right v) = return v
hoistError (Left err) = do
    liftIO $ print err
    abort

evalDef :: Gamma -> Either (ParseError Char Dec) (String, Term) -> Gamma
evalDef en (Left _) = en
evalDef en (Right (nm, ex)) = ctx'
    where (_, ctx') = runDynamic en nm ex

exec :: T.Text -> Repl ()
exec source = do
    st <- get
    mod' <- hoistError $ parseProgram "file" source
    let st' = st { termctx = termctx st, gammactx = foldl' evalDef (gammactx st) mod' }
    put st'

cmd :: String -> Repl ()
cmd source = exec $ T.pack source

-- Tab Completion: return a completion for partial words entered
comp :: (Monad m, MonadState Ctx m) => WordCompleter m
comp n = do
  let names = [""] -- placeholder
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

options :: [(String, [String] -> Repl ())]
options = [
    ("help", help)  -- :help
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [ (":load"  , fileCompleter) ]

completer :: CompleterStyle (StateT Ctx IO)
completer = Prefix (wordCompleter comp) defaultMatcher

repl :: IO ()
repl = flip evalStateT emptyContext $ evalRepl ">>> " cmd options completer ini

