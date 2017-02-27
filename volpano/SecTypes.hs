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
secCheck (Var _ l) = do
    tell ["var"]
    return l
secCheck (Num _ l) = do
    tell ["num"]
    return l
secCheck (Op op n1 n2) = do
    tell ["Op " ++ show op]
    n1' <- secCheck n1
    n2' <- secCheck n2
    return $ join n1' n2'
secCheck (BoolExpr _ l) = do
    tell ["bool"]
    return l
secCheck (Skip l) = do
    tell ["skip"]
    return l
secCheck (Seq e1 e2) = do
    tell ["seq"]
    _ <- secCheck e1
    secCheck e2
secCheck (Assign var val) = do
    tell ["assign"]
    var' <- secCheck var
    val' <- secCheck val
    if var' `confines` val'
       then return val'
       else throwError $ flowError val' var'
secCheck (IfThenElse b c1 c2) = do
    tell ["ifThenElse"]
    b' <- secCheck b
    case (c1, c2) of 
        (Assign{}, Assign{}) -> do
            c1' <- secCheck c1
            c2' <- secCheck c2
            let arms = c1' `meet` c2'
             in if b' `isSubTypeOf` arms
                   then return $ b' `join` arms
                   else throwError $ flowError b' arms
        (_, _) -> throwError "arms of conditional can only be assignments in this simple language"
secCheck err = throwError $ show err -- this should be unreachable

flowError :: Label -> Label -> String
flowError e1 e2 = "not typeable: implicit flow between " ++ show e1 ++ " and " ++ show e2

isSubTypeOf :: Label -> Label -> Bool
l1 `isSubTypeOf` l2 = l1 `eq` l2

confines :: Label -> Label -> Bool
confines l1 l2 = l2 `isSubTypeOf` l1
