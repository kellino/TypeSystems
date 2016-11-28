module Eval where

import Syntax
import TypeCheck

import Control.Monad.Except
import Unbound.Generics.LocallyNameless

type Eval a = ExceptT String FreshM a

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal TmAbs{} = True
isVal TmNumber{} = True
isVal _ = False

runEval :: Term -> Either String Term
runEval = runFreshM . runExceptT . eval

eval :: Term -> Eval Term
eval TmError = throwError "error value encountered"
eval v@TmVar{} = return v
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval n@TmNumber{} = return n
eval l@(TmAbs _) = return l
eval (TmApp TmError _) = return TmError
eval (TmApp t1 TmError) | isVal t1 = return TmError
eval (TmApp e1 e2) = do
    e2' <- eval e2
    case e1 of
        (TmAbs bnd) -> do
            ((x,_), body) <- unbind bnd
            let body' = subst x e2' body
            eval body'
        _ -> throwError "!"
eval (TmIf b t1 t2) = do
    b' <- eval b
    case b' of
         TmTrue ->  eval t1
         TmFalse -> eval t2
eval x = throwError $ show x

tyAndEval :: Term -> Either String (Term, Ty)
tyAndEval t = do
    ty <- runTypeOf t
    t' <- runEval t
    return (t', ty)
