module TypeCheck where

import Syntax
import Control.Monad.Except
import Control.Monad.Identity


type Type a = ExceptT UnTypeable Identity a

runTypeOf :: Expr -> Either UnTypeable Ty
runTypeOf = runIdentity . runExceptT . typeof

-- this checks concrete types, not security types, which is done in a separate pass
typeof :: Expr -> Type Ty
typeof Assign{} = return TyUnit
typeof Skip{} = return TyUnit
typeof (BoolExpr _ _) = return TyBool 
typeof (Num _ _) = return TyNum 
typeof (Op op n1 n2) = do
    t1 <- typeof n1
    t2 <- typeof n2
    if (t1 == TyNum) && (t2 == TyNum)
       then return TyNum
       else throwError $ "UnTypeable: cannot apply " ++ show op ++ " to operands of types " ++ show t1 ++ " and " ++ show t2
typeof (Seq e1 e2) = do
    _ <- typeof e1 -- check and then throw away
    typeof e2
typeof err = throwError $ "This is either not (yet) typeable :( or you've made a mistake\n" ++ show err
