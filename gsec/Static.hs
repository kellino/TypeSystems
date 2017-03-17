{-# LANGUAGE ViewPatterns #-}

module Static where

import Lattice
import Syntax
import Gamma

import Unbound.Generics.LocallyNameless
import Control.Monad.Except hiding (join)
import Control.Monad.Reader hiding (join)
import Control.Monad.Identity hiding (join)

type StaticType = FreshMT (ReaderT Gamma (ExceptT String Identity))

runStatic :: Gamma -> Term -> Either String GType
runStatic env t = runIdentity . runExceptT $ runReaderT (runFreshMT $ typeOf t) env

typeOf :: Term -> StaticType GType
typeOf (Var n) = do
    env <- ask
    let nty = lookupType n env
    return nty
typeOf (Val _ t) = return t
typeOf (IfThenElse b a1 a2) = do
    b' <- typeOf b
    case b' of
         TyBool{} -> do
             a1' <- typeOf a1
             a2' <- typeOf a2
             return a1'
         _ -> throwError "arms of conditional must be a boolean expression"
typeOf _ = throwError "unrecognized"


gradJoin :: GType -> GType -> StaticType GType
gradJoin (TyBool l1) (TyBool l2) = return $ TyBool (join l1 l2)
-- gradJoin (TyArr s11 s12 l) (TyArr s21 s22 l') = return $ TyArr (meet s11 s21) (join s12 s22) (join l l')

