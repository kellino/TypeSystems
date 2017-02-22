{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

data TypeEnv = TypeEnv { types :: [(String, Ty)] } deriving Show

runTypeOf :: Env -> TypeEnv -> Term -> Either String Ty
runTypeOf env ascripts m = runIdentity . runExceptT $ evalStateT (runReaderT (runFreshMT $ typeof m) env) ascripts
