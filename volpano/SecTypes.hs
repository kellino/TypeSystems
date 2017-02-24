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
    _ <- secCheck e1
    secCheck e2
-- assign is contravariant !!
secCheck (Assign var val) = do
    var' <- secCheck var
    val' <- secCheck val
    if val' `eq` var' 
       then return  (join var' val')
       else throwError $ "not typeable: " ++ show var' ++ " and " ++ show val'
secCheck (IfThenElse b c1 c2) = do
    b' <- secCheck b
    c1' <- secCheck c1
    c2' <- secCheck c2
    if (b' `eq` c1') && (c1' `eq` c2') 
       then return c1'
       else throwError "not typeable"
secCheck err = throwError $ show err
