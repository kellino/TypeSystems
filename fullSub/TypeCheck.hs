{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax
import Environment

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map as M

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
typeof TmString{} = return TyString
typeof TmFloat{} = return TyFloat
typeof (TmFloatTimes n1 n2) = do 
    n1' <- typeof n1
    n2' <- typeof n2
    if (n1' == n2') && n1' == TyFloat 
       then return TyFloat
       else throwError $ "expecting two numbers, but got " ++ show n1' ++ " and " ++ show n2'
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
    if b' `isSubTypeOf` TyBool
       then do
           e1' <- typeof e1
           e2' <- typeof e2
           joinTypes e1' e2'
       else throwError "guard of conditional is not a boolean"
typeof (TmLet bnd) = do
    ((x, unembed -> t1), t2) <- unbind bnd
    let new = subst x t1 t2
    typeof new
typeof (TmApp e1 e2) = 
    case e1 of 
        (TmAbs bnd) -> do
            e2' <- typeof e2
            ((x,_), _) <- unbind bnd
            local (\m@Env{ctx = cs} -> m { ctx = (show x, e2'):cs }) $ do
                e1' <- typeof e1
                case e1' of
                    (TyArr l r) -> if e2' `isSubTypeOf` l then return r else throwError "annotation mismatch"
                    err -> throwError $ "expecting an arrow type, but got " ++ show err
        app@TmApp{} -> typeof app
        x -> throwError $ "weird! " ++ show x
typeof (TmFix t) = typeof t
typeof (TmRecord fields) = do
    tys' <- mapM (typeof . snd) fields
    return $ TyRecord tys'
typeof (TmProj flds p) = 
    case flds of
        (TmRecord fs) -> do
            let found = lookup p fs
            case found of
                 Nothing -> throwError $ show p ++ " not found in the record"
                 Just p' -> typeof p'
        _ -> throwError "expecting a record type"
   

isSubTypeOf :: Ty -> Ty -> Bool
isSubTypeOf _ TyTop = True
isSubTypeOf TyBot _ = True
isSubTypeOf (TyArr s1 s2) (TyArr t1 t2) = isSubTypeOf t1 s1 && isSubTypeOf s2 t2
isSubTypeOf (TyRecord t1) (TyRecord t2) = undefined
isSubTypeOf x y
    | x == y     = True
    | otherwise = False

joinTypes :: Ty -> Ty -> TypeM Ty
joinTypes t1 t2
    | t1 `isSubTypeOf` t2 = return t2
    | t2 `isSubTypeOf` t1 = return t1
    | otherwise           = return TyTop
