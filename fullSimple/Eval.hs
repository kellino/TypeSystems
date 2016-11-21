module Eval where

import Syntax
import TypeCheck

import Unbound.Generics.LocallyNameless
import Control.Monad.Except

type Eval a = ExceptT Error FreshM a

isNumerical :: Term -> Bool
isNumerical TmZero = True
isNumerical (TmSucc t) = isNumerical t
isNumerical _ = False

eval :: Term -> Eval Term
eval TmUnit = return TmUnit
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval TmZero = return TmZero
eval t@(TmRecord _ "") = return t
eval (TmRecord r n) = do
    let found = lookup n r
    case found of
         Nothing -> throwError "field not found in record"
         Just t -> return t
eval d@TmFloat{} = return d
eval s@TmString{} = return s
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
eval v@Var{} = return v
eval a@Abs{} = return a
-- eval (Let n t1 t2) = undefined
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
