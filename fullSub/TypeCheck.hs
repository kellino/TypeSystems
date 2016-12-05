{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax
import Environment

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

isVar :: Term -> Bool
isVar TmVar{} = True
isVar _       = False

runTypeOf :: Env -> TypeEnv -> Term -> Either String Ty
runTypeOf env ascripts m = runIdentity . runExceptT $ evalStateT (runReaderT (runFreshMT $ typeof m) env) ascripts

typeof :: Term -> TypeM Ty
typeof TmAscription{} = return TyUnit
typeof TmError = return TyBot
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof (TmVar x) = do
    (Env env) <- ask
    let found = lookup (decrement x) env
    case found of
         Nothing -> throwError $ "variable is not bound in environment " ++ show (decrement x) ++ " " ++ show env
         Just r -> return r
typeof TmZero = return TyNat 
typeof (TmIsZero t) = do
    t' <- typeof t
    case t' of
         TyNat -> return TyBool
         _ -> throwError "argument to iszero is not a number"
typeof (TmAbs bnd) = do
    ((_, unembed -> tys), body) <- unbind bnd
    tys' <- mapM fromTypeEnv tys
    case last tys' of
        arr@(TyArr _ r) -> do
            body' <- typeof body
            if body' `isSubTypeOf` r
               then return arr
               else throwError "annotation error"
        retty -> do
            body' <- typeof body
            if body' `isSubTypeOf` retty 
               then return $ foldl1 TyArr tys' 
               else throwError $ "annotated type " ++ show retty ++ " is different from inferred type " ++ show body'
typeof (TmSucc t) = typeof t
typeof (TmPred t) = typeof t
typeof (TmIf b e1 e2) = do
    b' <- typeof b
    case b' of
         TyBool -> do
             e1' <- typeof e1
             e2' <- typeof e2
             if e1' == e2' then return e2' else throwError "arms of conditional are different types"
         _ -> throwError "guard of conditional is not a boolean"
typeof (TmApp e1 e2) = 
    case e1 of 
        (TmAbs bnd) -> do
            e2' <- typeof e2
            ((x,_), _) <- unbind bnd
            local (\m@Env{ctx = cs} -> m { ctx = (show x, e2'):cs }) $ do
                e1' <- typeof e1
                case e1' of
                    (TyArr l r) -> if l == e2' then return r else throwError "annotation mismatch"
                    err -> throwError $ "expecting an arrow type, but got " ++ show err
        _ -> throwError "expecting a lambda abstraction as the first value"

isSubTypeOf :: Ty -> Ty -> Bool
isSubTypeOf _ TyTop = True
isSubTypeOf TyBot _ = True
isSubTypeOf (TyArr s1 s2) (TyArr t1 t2) = isSubTypeOf t1 s1 && isSubTypeOf s2 t2
-- isSubTypeOf r1@TyRecord{} r2@TyRecord{} = recordCheck r1 r2
isSubTypeOf x y
    | x == y     = True
    | otherwise = False
