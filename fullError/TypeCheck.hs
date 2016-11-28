{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.Identity

type TypeM = FreshMT (ExceptT String Identity)

runTypeOf :: Term -> Either String Ty
runTypeOf = runIdentity . runExceptT . runFreshMT . typeof

typeof :: Term -> TypeM Ty
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmNumber{} = return TyNum
typeof (TmAbs bnd) = do
    ((_, unembed -> Annot t), body) <- unbind bnd
    t1 <- typeof body
    case t of
         Nothing -> throwError "missing type annotation"
         Just (Type t') -> return $ TyArr t' t1
         err -> throwError $ "badly formed type " ++ show err
typeof TmError = return TyBot
typeof (TmIf b t1 t2) = do
    b' <- typeof b
    case b' of
         TyBool -> evalBool t1 t2
         TyBot  -> evalBool t1 t2
         _ -> throwError "guard of conditional is not a boolean"
typeof (TmApp e1 e2) = do
    e1' <- typeof e1
    case e1' of
        (TyArr l r) -> do
            e2' <- typeof e2
            if l == e2' 
               then return r
               else throwError "argument mismatch"
        TyBot -> typeof e2
        _ -> throwError "expecting an arrow type"
typeof x = throwError $ show x


------------------------------------
-- HELPER FUNCTIONS FOR SUBTYPING --
------------------------------------

evalBool :: Term -> Term -> TypeM Ty
evalBool t1 t2 = do
    t1' <- typeof t1
    t2' <- typeof t2
    if t1' == t2'
       then return t2'
       else throwError "arms of conditional do not match"

