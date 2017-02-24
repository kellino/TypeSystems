module Eval where

import Syntax
import Control.Monad.Except
import Control.Monad.Identity

type Eval a = ExceptT Error Identity a

runEval :: Expr -> Either Error Expr
runEval = runIdentity . runExceptT . eval

eval :: Expr -> Eval Expr
eval t@(BoolExpr TmTrue _) = return t
eval f@(BoolExpr TmFalse _) = return f
eval s@Skip{} = return s
-- these two to do
eval a@Assign{} = return a
eval app@App{} = throwError $ show app
eval (IfThenElse b e1 e2) = do
    b' <- eval b
    case b' of
        (BoolExpr TmTrue _) -> eval e1
        _                 -> eval e2
eval v@Var{} = return v
eval n@Num{} = return n
eval (Seq e1 e2) = do
    _ <- eval e1
    eval e2
eval (While b e) = do
    b' <- eval b
    case b' of
        (BoolExpr TmTrue _) -> do
            _ <- eval e
            eval (While b e)
        _ -> return $ Skip High 
eval (Op op n1 n2) = 
    case op of
         Add -> return $ n1 `add` n2
         Sub -> return $ n1 `sub` n2
         Equal -> return $ comp (==) n1 n2
         LessThan -> return $ comp (<) n1 n2
         LessThanEq -> return $ comp (<=) n1 n2
eval err = throwError $ "The expression " ++ show err ++ " cannot be evaluated"

add :: Expr -> Expr -> Expr
add (Num n1 l) (Num n2 _) = Num (n1 + n2) l

sub :: Expr -> Expr -> Expr
sub (Num n1 l) (Num n2 _) = Num (n1 - n2) l

comp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> Expr
comp f (Num n1 l) (Num n2 _) = if n1 `f` n2 then BoolExpr TmTrue l else BoolExpr TmFalse l

