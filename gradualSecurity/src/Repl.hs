{-# LANGUAGE FlexibleContexts #-}

module Repl where

import Gamma 
import Syntax
import Processor
import PrettyPrint

import Text.PrettyPrint.ANSI.Leijen (putDoc, hardline, (<>))
import Control.Monad.Catch (MonadThrow, throwM)
import System.Exit (exitSuccess)
import System.Console.Repline
import Control.Monad.State.Strict
import System.Directory (doesFileExist)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (isPrefixOf)

instance (Monad m) => MonadThrow (HaskelineT m) where
    throwM = undefined

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to gradSec!\n\n" 

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [ (":load"  , fileCompleter) ]

browse :: [String] -> Repl ()
browse _ = do
    st <- get
    let (Gamma ctx) = gamma st
    liftIO $ mapM_ (putDoc . ppstatic) (M.toList ctx)

-- :type command
typeof :: [String] -> Repl ()
typeof args = 
    if null args
       then liftIO $ putStrLn "you must enter the name of a function"
       else do
          st <- get
          let arg = unwords args
          let (Gamma gm) = gamma st
          case M.lookup arg gm of
            Just val -> liftIO $ putDoc $ (display val) <> hardline
            Nothing  -> liftIO $ putStrLn $ show arg ++ " is not in scope"

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

load :: [String] -> Repl ()
load args =
    if null args
       then liftIO $ putStrLn "you must enter a filename"
       else do 
            tr <- liftIO $ doesFileExist (unwords args)
            if tr then do
                    contents <- liftIO $ T.readFile (unwords args)
                    processStatic contents False
                  else liftIO $ putStrLn $ "the file does not exist: " ++ unwords args

comp :: (Monad m, MonadState Ctx m) => WordCompleter m
comp n = do
  let names = [":load", ":help", ":quit", ":browse", ":typeof"] -- placeholder
  return $ filter (isPrefixOf n) names

options :: [(String, [String] -> Repl ())]
options = [
        ("browse" , browse)
      , ("typeof", typeof)
      , ("quit"   , quit)
      , ("?"      , help)
      , ("help"   , help) -- alternative
      , ("load"   , load)
      ]

completer :: CompleterStyle (StateT Ctx IO)
completer = Prefix (wordCompleter comp) defaultMatcher

cmd :: String -> Repl ()
cmd source = processDynamic (T.pack source)

repl :: IO ()
repl = flip evalStateT initCtx $ evalRepl "λsec > " cmd options completer ini
