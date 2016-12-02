{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax

import Data.Maybe
import Control.Monad.State
import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map

isNumeric :: Term -> Bool
isNumeric TmZero     = True
isNumeric (TmSucc t) = isNumeric t
isNumeric (TmPred t) = isNumeric t
isNumeric _          = False

type TypeM = FreshMT (StateT TypeEnv (ExceptT String Identity))

type TypeEnv = Map.Map String Ty

initEnv :: TypeEnv
initEnv = Map.fromList [("Nat", TyNat), ("Bool", TyBool), ("Any", TyTop)]

runTypeOf :: TypeEnv -> Term -> Either String Ty
runTypeOf env m = runIdentity . runExceptT $ evalStateT (runFreshMT $ typeof m) env

typeof :: Term -> TypeM Ty
typeof v@TmVar{} = return TyTop
typeof TmError = return TyBot
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmZero = return TyNat
typeof (TmRecord flds) = do
    tys <- mapM (typeof . snd) flds
    return $ TyRecord $ zip (map fst flds) tys
typeof (TmProj (TmRecord rec) f) = do
    let found = filter ((== f) . fst) rec 
    if null found 
       then throwError "field does not exist"
       else typeof . snd . head $ found
typeof (TmSucc t) 
    | isNumeric t = return TyNat
    | otherwise = throwError $ "argument to succ " ++ show t ++ " is not a number"
typeof (TmAbs bnd) = do
    ctx <- get
    ((x, unembed -> tys), body) <- unbind bnd
    let arg = fromJust $ Map.lookup (head tys) ctx
    let ret = fromJust $ Map.lookup (last tys) ctx
    body' <- typeof body
    if body' `isSubTypeOf` ret
       then return $ TyArr arg ret
       else throwError "the inferred return type does not match the annotation"
typeof (TmIf b t1 t2) = do
    b' <- typeof b
    case b' of
         TyBool -> typeBoolean t1 t2
         TyTop  -> typeBoolean t1 t2
         _ -> throwError ""
typeof (TmApp e1 e2) = 
    case e1 of
        TmAbs{} -> typeAbsApp e1 e2
        app@TmApp{} -> typeof app 
        _ -> throwError "expecting a lambda expression"
typeof x = throwError $ "type check error: " ++ show x

isSubTypeOf :: Ty -> Ty -> Bool
isSubTypeOf _ TyTop = True
isSubTypeOf TyBot _ = True
isSubTypeOf (TyArr s1 s2) (TyArr t1 t2) = isSubTypeOf t1 s1 && isSubTypeOf s2 t2
isSubTypeOf r1@TyRecord{} r2@TyRecord{} = recordCheck r1 r2
isSubTypeOf x y
    | x == y     = True
    | otherwise = False

recordCheck :: Ty -> Ty -> Bool
recordCheck (TyRecord r1) (TyRecord r2) = r2' `Map.isSubmapOf` r1'
    where r1' = Map.fromList r1
          r2' = Map.fromList r2

typeAbsApp :: Term -> Term -> TypeM Ty
typeAbsApp (TmAbs bnd) e2 = do
    ctx <- get
    ((x, unembed -> tys), body) <- unbind bnd
    body' <- typeof body
    e2' <- typeof e2
    let ret = fromJust $ Map.lookup (last tys) ctx
    let var = fromJust $ Map.lookup (head tys) ctx
    if (body' `isSubTypeOf` ret) && (e2' `isSubTypeOf` var) 
       then return ret 
       else throwError $ show body' ++ " " ++ show ret ++ " " ++ show e2' ++ " " ++ show var

typeBoolean :: Term -> Term -> TypeM Ty
typeBoolean t1 t2 = do
    t1' <- typeof t1
    t2' <- typeof t2
    if t2' == TyBot 
       then return t1'
       else if t1' `isSubTypeOf` t2' 
               then return t2' 
               else throwError "arms of conditional are of different types"
