{-# LANGUAGE FlexibleContexts #-}

module Environment where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity

type TypeM = FreshMT (ReaderT Env (ExceptT String Identity))

data Env = Env { ctx :: [(TName, Ty)] } deriving Show

emptyEnv :: Env
emptyEnv = Env { ctx = [] }

lookUpVar :: (MonadReader Env m, MonadError String m) => TName -> m Ty
lookUpVar v = do
    ctx' <- asks ctx
    let res = lookup v ctx'
    case res of
         Nothing -> throwError "unbound variable"
         Just r -> return r

extendCtx :: MonadReader Env m => (TName, Ty) -> m a -> m a
extendCtx d = local (\m@Env{ ctx = cs } -> m { ctx = d:cs })
