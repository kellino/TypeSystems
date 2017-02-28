{-# LANGUAGE FlexibleContexts #-}

module SecTypes where

import Syntax
import Control.Monad.Except hiding (join)
import Control.Monad.Identity hiding (join)
import Control.Monad.Writer hiding (join)

type Flow = String
type Step = String

type SecType a = WriterT [Step] (ExceptT Flow Identity) a

runSecTypeCheck :: Expr -> Either Flow (Label, [Step])
runSecTypeCheck =  runIdentity . runExceptT . runWriterT . secCheck 

secCheck :: Expr -> SecType Label
secCheck (Var n l) = do
    tell ["γ ⊦ " ++ n ++ " : " ++ show l]
    return l
secCheck (Num n l) = do
    tell ["num " ++ show n ++ " " ++ show l]
    return l
secCheck (Op op n1@(Num n _) n2@(Num n' _)) = do
    n1' <- secCheck n1
    n2' <- secCheck n2
    tell [show n ++ " " ++ show op ++ " " ++ show n' ++ " : " ++ show (join n1' n2')]
    return $ join n1' n2'
secCheck (Op op n1@(Var n _) n2@(Var n' _)) = do
    n1' <- secCheck n1
    n2' <- secCheck n2
    tell [n ++ " " ++ show op ++ " " ++ n' ++ " : " ++ show (join n1' n2')]
    return $ join n1' n2'
secCheck (BoolExpr _ l) = do
    tell ["bool " ++ show l]
    return l
secCheck (Skip l) = do
    tell ["skip " ++ show l]
    return l
secCheck (Seq e1 e2) = do
    tell ["seq"]
    _ <- secCheck e1
    secCheck e2
secCheck (Assign var@(Var n _) val@(Var n' _)) = do
    var' <- secCheck var
    val' <- secCheck val
    if var' `confines` val'
       then do 
           tell [n ++ " := " ++ n' ++ " : " ++ show (var' `join` val') ++ " cmd"]
           tell ["assign"]
           return var'
       else throwError ""
secCheck (IfThenElse b c1 c2) = do
    b' <- secCheck b
    case (c1, c2) of 
        (Assign{}, Assign{}) -> do
            c1' <- secCheck c1
            c2' <- secCheck c2
            let arms = c1' `meet` c2'
             in if b' `isSubTypeOf` arms
                   then do
                       tell ["if...then...else" ++ show (b' `join` arms) ++ " Cmd"]
                       return $ b' `join` arms
                   else throwError $ flowError b' arms
        (_, _) -> throwError "arms of conditional can only be assignments in this simple language"
secCheck err = throwError $ show err -- this should be unreachable

flowError :: Label -> Label -> String
flowError e1 e2 = "not typeable: implicit flow between " ++ show e1 ++ " and " ++ show e2

isSubTypeOf :: Label -> Label -> Bool
l1 `isSubTypeOf` l2 = l1 `eq` l2

confines :: Label -> Label -> Bool
confines l1 l2 = l2 `isSubTypeOf` l1
