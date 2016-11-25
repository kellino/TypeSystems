{-# LANGUAGE ViewPatterns #-}

module Eval where

import Syntax
import TypeCheck
import Environment

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Data.Maybe (fromJust)

type Eval a = ExceptT String FreshM a

isNumerical :: Term -> Bool
isNumerical TmZero = True
isNumerical (TmSucc t) = isNumerical t
isNumerical _ = False

eval :: Term -> Eval Term
eval TmZero = return TmZero
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval TmUnit = return TmUnit
eval (TmSucc t) = do
    t' <- eval t
    return $ TmSucc t'
eval (TmPred TmZero) = return TmZero
eval (TmPred t) = do
    t' <- eval t
    return $ TmPred t'
eval (TmIsZero TmZero) = return TmTrue
eval (TmIsZero (TmSucc t)) | isNumerical t = return TmFalse
eval (TmIsZero t) = do
    t' <- eval t
    return $ TmIsZero t'
eval s@TmString{} = return s
eval f@TmFloat{}  = return f
eval r@TmRecord{} = return r
eval (TmTimesFloat (TmFloat x) (TmFloat y)) = return $ TmFloat (x * y)
eval v@(Var _) = return v
eval l@Lam{} = throwError $ "the problem is here: " ++ show l
eval (If b t1 t2) = do
    b' <- eval b
    case b' of
         TmTrue -> eval t1
         TmFalse -> eval t2
         x -> throwError $ show x 
eval (Let bnd) = do
    ((n, unembed -> t1), t2) <- unbind bnd
    t1' <- eval t1
    let body = subst n t1' t2
    eval body
eval (TmProjection (TmRecord xs) v) = return $ fromJust $ lookup v xs -- this is ok because the typechecker has confirmed the existence of v
eval (App e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case v1 of
        (Lam bnd) -> do
            ((x, _), body) <- unbind bnd
            let body' = subst x v2 body
            eval body'
        _ -> throwError "first expression is not a lambda abstraction"
eval x = throwError $ "no rule applies: " ++ show x

runEval :: Term -> Either String Term
runEval = runFreshM . runExceptT . eval

typeAndEval :: Env -> Term -> Either String (Term, Ty)
typeAndEval env t =
    case t of
        pr@TmProjection{} -> do
            _ <- runTypeOf env t
            t' <- runEval pr
            typeAndEval env t'
        _ -> do
            ty <- runTypeOf env t
            ev <- runEval t
            return (ev, ty)
