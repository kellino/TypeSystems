{-# LANGUAGE ViewPatterns #-}

module TypeCheck (runTypeOf) where

import Syntax
import Environment

import Unbound.Generics.LocallyNameless
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

isNumeric :: Term -> Bool
isNumeric TmZero     = True
isNumeric (TmSucc t) = isNumeric t
isNumeric (TmPred t) = isNumeric t
isNumeric _          = False

typeof :: Term -> TypeM Ty
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmUnit = return TyUnit
typeof TmZero = return TyNat
typeof TmFloat{} = return TyFloat
typeof TmString{} = return TyString
typeof (TmTimesFloat t1 t2) = do
    t1' <- typeof t1
    t2' <- typeof t2
    if (t1' == t2') && t1' == TyFloat
       then return TyFloat
       else throwError "timesfloat requires two floats as arguments"
typeof (Var v) = lookUpVar v
typeof (Let bnd) = do
    ((_, unembed -> t1), _) <- unbind bnd
    typeof t1
typeof (TmRecord r) = do
    r' <- mapM (typeof . snd) r
    return $ TyRecord r'
typeof (TmProjection (TmRecord xs) v) = do
    let found = lookup v xs
    case found of
         Nothing -> throwError "var not found in record"
         Just t -> typeof t
typeof (Fix e) = typeof e
typeof (TmProjection _ _) = throwError "first element in a projection must be a record"
typeof (If b t1 t2) = do
    b' <- typeof b
    if b' == TyBool 
       then do
           t1' <- typeof t1
           t2' <- typeof t2
           if t1' == t2' 
              then return t2'
              else throwError "arms of conditional are of different types"
       else throwError "guard of expression is not a boolean"
typeof (TmIsZero t) = 
    if isNumeric t 
       then return TyBool
       else throwError $ "argument to isZero " ++ show t ++ " is not a number"
typeof (TmPred t) = 
    if isNumeric t
       then return TyNat
       else throwError $ "argument of pred " ++ show t ++ " is not a number"
typeof (TmSucc t) = 
    if isNumeric t 
       then return TyNat 
       else throwError $ "argument of succ " ++ show t ++ " is not a number"
typeof (Lam bnd) = do
    ((_, unembed -> Annot t), _) <- unbind bnd
    case t of
         Nothing -> throwError "missing type annotation"
         Just (Type t') -> return t'
         Just x -> throwError $ show x
typeof (App e1 _) = typeof e1 -- placeholder
typeof x = throwError $ show x

runTypeOf :: Env -> Term -> Either String Ty
runTypeOf env m = runIdentity $ runExceptT $ runReaderT (runFreshMT $ typeof m) env
