module Arith where

import Control.Monad.Except
import Control.Monad.Identity

type Eval a = ExceptT String Identity a

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term

instance Show Term where
    show TmTrue  = "T"
    show TmFalse = "F"
    show TmZero  = "Z"
    show (TmSucc t1) = "(S " ++ show t1 ++ ")"
    show (TmPred t1) = "(P " ++ show t1 ++ ")"
    show (TmIf t1 t2 t3) = "If " ++ show t1 ++ show t2 ++ show t3
    show (TmIsZero t1) = "Z? " ++ show t1

isNumeric :: Term -> Bool
isNumeric TmZero      = True
isNumeric (TmSucc t1) = isNumeric t1
isNumeric _           = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumeric t

runEval :: Eval a -> Either String a
runEval e = runIdentity $ runExceptT e

run :: Term -> IO ()
run e = 
    case runEval (eval e) of
         Left err -> print err
         Right r -> print r

eval :: Term -> Eval Term
eval (TmIsZero TmZero) = return TmTrue
eval (TmIsZero (TmSucc _)) = return TmFalse
eval (TmIsZero t1) = do
    t1' <- eval t1
    eval (TmIsZero t1')
eval (TmIf TmTrue t2 _)  = return t2
eval (TmIf TmFalse _ t3) = return t3
eval (TmIf b t1 t2) = do
    b' <- eval b
    eval (TmIf b' t1 t2)
eval z@(TmSucc TmZero) = return z
eval (TmSucc t1) = do
    t1' <- eval t1
    return $ TmSucc t1'
eval (TmPred TmZero) = return TmZero 
eval (TmPred (TmSucc t1)) = 
    if isNumeric t1 
       then return t1 
       else throwError $ "cannot get successor of " ++ show t1
eval (TmPred t1) = 
    case runEval $ eval t1 of
         Left _ -> throwError $ "cannot get predecessor of " ++ show t1
         Right t1' -> eval (TmPred t1')
eval _ = throwError "what have you done?"
