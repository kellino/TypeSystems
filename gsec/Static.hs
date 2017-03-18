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
typeOf (Val _ t) = return $ TyBool t
typeOf (IfThenElse b a1 a2) = do
    b' <- typeOf b
    case b' of
         (TyBool l) -> do
             a1' <- typeOf a1
             a2' <- typeOf a2
             arms@(TyBool l') <- gradJoin a1' a2'
             if l `flowsTo` l'
                then gradJoin b' arms
                else throwError "flow not allowed"
         _ -> throwError "arms of conditional must be a boolean expression"
typeOf _ = throwError "unrecognized"

gradJoin :: GType -> GType -> StaticType GType
gradJoin (TyBool l1) (TyBool l2) = return $ TyBool (l1 \/ l2)
gradJoin (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    t1 <- gradMeet s11 s21 
    t2 <- gradJoin s12 s22
    return $ TyArr t1 t2 (l /\ l')
gradJoin _ _ = throwError "undefined"

gradMeet :: GType -> GType -> StaticType GType
gradMeet (TyBool l1) (TyBool l2) = return $ TyBool (l1 /\ l2)
gradMeet (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    t1 <- gradJoin s11 s21
    t2 <- gradMeet s12 s22
    return $ TyArr t1 t2 (l /\ l')
gradMeet _ _ = throwError "undefined"
