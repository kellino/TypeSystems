{-# LANGUAGE ViewPatterns #-}

module Eval where

import Syntax

import Data.Maybe (fromJust)
import Unbound.Generics.LocallyNameless
import Control.Monad.Except

type Eval a = ExceptT String FreshM a

runEval :: Term -> Either String Term
runEval = runFreshM . runExceptT . eval

eval :: Term -> Eval Term
eval TmError = throwError "Exception encountered"
eval re@TmRecord{} = return re
eval str@TmString{} = return str
eval v@TmVar{} = return v
eval l@TmAbs{} = return l
eval a@TmAscription{} = return a
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval TmZero = return TmZero
eval (TmIsZero TmZero) = return TmTrue
eval (TmIsZero _) = return TmFalse
eval f@TmFloat{} = return f
eval s@TmSucc{} =  return s
eval p@TmPred{} = return p
eval (TmFloatTimes (TmFloat x) (TmFloat y)) = return $ TmFloat (x * y)
eval (TmLet bnd) = do
    ((x, unembed -> t1), t2) <- unbind bnd
    t1' <- eval t1
    let body = subst x t1' t2
    eval body
eval (TmFix e) = eval (TmApp e (TmFix e))
eval (TmIf b t1 t2) = do
    b' <- eval b
    case b' of
         TmTrue -> eval t1
         TmFalse -> eval t2
         x -> throwError $ show x 
eval (TmApp e1 e2) = do
    v2 <- eval e2
    case e1 of
        (TmAbs bnd) -> do
            ((x, _), body) <- unbind bnd
            let body' = subst x v2 body
            eval body'
        _ -> throwError "first expression is not a lambda abstraction"
eval (TmProj (TmRecord fields) p) = return $ fromJust $ lookup p fields
eval (TmProj _ _) = throwError "projection can only be applied to records"
eval x = throwError $ "not yet implemented: " ++ show x
