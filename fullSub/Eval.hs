module Eval where

import Syntax
import TypeCheck

import Data.Maybe
import Unbound.Generics.LocallyNameless
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

type Eval = FreshMT (ReaderT Env (ExceptT String Identity))

newtype Env = Env { vars :: [Term] }
    deriving Show

isNumerical :: Term -> Bool
isNumerical TmZero     = True
isNumerical (TmSucc t) = isNumerical t
isNumerical (TmPred t) = isNumerical t
isNumerical _          = False

emptyCtx :: Env
emptyCtx = Env { vars = [] }

eval :: Term -> Eval Term
eval TmError = throwError "error value encountered\n"
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval TmZero = return TmZero
eval r@TmRecord{} = return r
eval (TmIsZero TmZero) = return TmTrue
eval (TmIsZero (TmSucc t)) | isNumerical t = return TmFalse
eval (TmIsZero t) = eval t
eval (TmSucc t) = do
    t' <- eval t
    return $ TmSucc t'
eval (TmPred TmZero) = return TmZero
eval (TmPred t) | isNumerical t = return t
eval (TmIf b t1 t2) = do
    b' <- eval b
    case b' of
         TmTrue -> eval t1
         TmFalse -> eval t2
         x -> throwError $ show x
eval (TmProj (TmRecord rec) f) = return $ fromJust $ lookup f rec
eval (TmApp e1 e2) = do
    e2' <- eval e2
    case e1 of
        (TmAbs bnd) -> do
            ((x, _), body) <- unbind bnd
            let body' = subst x e2' body
            eval body'
        x -> local (\ m@Env{ vars = xs } -> m { vars = x:xs }) (eval e1)
eval x = throwError $ show x

runEval :: Env -> Term -> Either String Term
runEval env m = runIdentity $ runExceptT $ runReaderT (runFreshMT (eval m)) env

typeAndEval :: Term -> Either String (Term, Ty)
typeAndEval t = do
    ty <- runTypeOf t
    t' <- runEval emptyCtx t
    return (t', ty)

