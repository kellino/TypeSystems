module SecTypes where

import Syntax
import Control.Monad.Except hiding (join)
import Control.Monad.Identity hiding (join)


type Flow = String

type SecType a = ExceptT Flow Identity a

runSecTypeCheck :: Expr -> Either Flow Label
runSecTypeCheck = runIdentity . runExceptT . secCheck

secCheck :: Expr -> SecType Label
secCheck (Var _ l) = return l
secCheck (Num _ l) = return l
secCheck (BoolExpr _ l) = return l
secCheck (Op _ n1 n2) = do
    n1' <- secCheck n1
    n2' <- secCheck n2
    return $ join n1' n2'
secCheck (Skip l) = return l
secCheck (Seq e1 e2) = do
    e1' <- secCheck e1
    e2' <- secCheck e2
    if e1' `confines` e2'
       then return e2'
       else throwError "not typeable"
secCheck (Assign var val) = do
    var' <- secCheck var
    val' <- secCheck val
    if var' `confines` val'
       then return val'
       else throwError $ "not typeable: implicit flow from " ++ show val' ++ " to " ++ show var'
secCheck (IfThenElse b c1 c2) = do
    b' <- secCheck b
    c1' <- secCheck c1
    c2' <- secCheck c2
    let arms = c1' `join` c2'
    if b' `isSubTypeOf` arms
       then return arms
       else throwError $ "Untypeable: implicit flow from " ++ show b' ++ " to " ++ show arms
secCheck err = throwError $ show err


isSubTypeOf :: Label -> Label -> Bool
isSubTypeOf l1 l2 = l1 `eq` l2

confines :: Label -> Label -> Bool
confines l1 l2 = l2 `eq` l1
