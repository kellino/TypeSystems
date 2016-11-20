module Eval where

import Syntax
import TypeCheck

import Unbound.Generics.LocallyNameless
import Control.Monad.Except

type Eval a = ExceptT Error FreshM a

eval :: Term -> Eval Term
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval v@Var{} = return v
eval a@Abs{} = return a
eval (If b t1 t2) = do
    b' <- eval b
    case b' of
         TmTrue -> eval t1
         TmFalse -> eval t2
         x -> throwError $ show x 
eval (App e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case v1 of
        (Abs bnd _) -> do
            (x, body) <- unbind bnd
            let body' = subst x v2 body
            eval body'
        _ -> throwError "application of non-lambda"

runEval :: Term -> Either Error Term
runEval t = do
    _ <- runTypeOf t
    runFreshM . runExceptT . eval $ t
