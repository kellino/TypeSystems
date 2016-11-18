module Eval where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except

type Eval a = ExceptT Error FreshM a

eval :: Term -> Eval Term
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval v@Var{} = return v
eval e@Abs{} = return e
eval (If b t1 t2) = do
    b' <- eval b
    case b' of
         TmTrue -> eval t1
         TmFalse -> eval t2
         x -> throwError $ show x ++ " is not a boolean."
eval (App e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case v1 of
        (Abs bnd) -> do
            (x, body) <- unbind bnd
            let body' = subst x v2 body
            eval body'
        _ -> fail "application of non-lambda"

runEval :: Term -> Either Error Term
runEval = runFreshM . runExceptT . eval
