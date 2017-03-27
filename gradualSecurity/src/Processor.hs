{-# LANGUAGE FlexibleContexts #-}

module Processor where

import Syntax
import Gamma
import StaticCheck
import PrettyPrint
import Parser
import DynamicCheck

import qualified Data.Text as T
import Data.Map as M hiding (foldl')
import Control.Monad.State.Strict
import Control.Monad (when)
import Control.Exception (Exception)
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Text.PrettyPrint.ANSI.Leijen (putDoc, hardline)
import Data.Either (rights)
import Data.Monoid ((<>))
import Data.List (foldl')
import System.Console.Repline


type Repl a = HaskelineT (StateT Ctx IO) a

------------
-- hoists --
------------

hoist :: (Monad m, MonadThrow m, Show e, Exception e) => Either e a -> m a
hoist (Right v) = return v
hoist (Left err) = throwM err

hoistReplError :: (Display e) => Either e a -> Repl a
hoistReplError (Right v) = return v
hoistReplError (Left err) = do
    liftIO . putDoc . display $ err
    abort

------------------------------
-- File and Line Processors --
------------------------------

-- | TODO 
-- the use of "rights" here in both process functions is very hacky and lazy. This needs to be
-- rewritten at some point.

processStatic:: (Monad m, MonadIO m, MonadThrow m, MonadState Ctx m) => T.Text -> Bool -> m ()
processStatic source static = do
    st <- get
    -- generate AST
    contents <- hoist $ parseProgram "<from file>" source
    -- run static checker over file.
    staticCtx <- hoist $ runOverFile st $ rights contents
    let (Gamma stx) = gamma staticCtx
    let (Gamma gm) = gamma st
    let st' = st { termenv = termenv st
                 , gamma = Gamma (stx <> gm) }
    put st'
    let (Gamma new) = gamma st'
    when static $ liftIO $ mapM_ (putDoc . ppstatic) (M.toList new)

processDynamic :: T.Text -> Repl ()
processDynamic line = do
    st <- get
    contents <- hoistReplError $ parseProgram "<stdin>" line
    staticCtx <- hoistReplError $ runOverFile st $ rights contents
    let (Gamma stx) = gamma staticCtx
    let (Gamma gm) = gamma st
    let st' = st { termenv = foldl' evalDef (termenv st) (rights contents)
                 , gamma = Gamma (stx <> gm) }
    put st'

    case Prelude.lookup "it" (rights contents) of
         Nothing -> return ()
         Just ex -> 
             case runDynamic (termenv st) "it" ex of
                  Left err -> do 
                    liftIO $ print err
                    abort
                  Right (val, _) -> showOutput (show val) st'

showOutput :: String -> Ctx -> Repl ()
showOutput arg st = 
    case M.lookup "it" (termenv st) of
         Just res -> liftIO $ putDoc (display res <> hardline)
         Nothing -> return ()
