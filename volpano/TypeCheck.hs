module TypeCheck where

import Syntax
import Control.Monad.Except
import Control.Monad.Identity


type Type a = ExceptT UnTypeable Identity a

runTypeOf :: Expr -> Either UnTypeable Ty
runTypeOf = runIdentity . runExceptT . typeof

-- this checks concrete types, not security types, which is done in a separate pass
typeof :: Expr -> Type Ty
typeof Var{} = return TyLoc
typeof (Assign var val) = do
    _ <- typeof var
    _ <- typeof val
    return TyUnit
typeof Skip{} = return TyUnit
typeof (BoolExpr _ _) = return TyBool 
typeof (Num _ _) = return TyNum 
-- this is not really correct, but good enough for the demo
typeof (Op op n1 n2) = do
    t1 <- typeof n1
    t2 <- typeof n2
    case (op, t1, t2) of
        (Add, TyNum, TyNum) -> return TyNum
        (Add, n1, n2) -> throwError $ "cannot add " ++ show n1 ++ " to " ++ show n2
        (Sub, TyNum, TyNum) -> return TyNum
        (Sub, _, _) -> throwError $ "cannot sub " ++ show n1 ++ " and " ++ show n2
        (Equal, _, _) -> return TyBool
        LessThanEq{} -> return TyBool
        (Op f x) -> throwError $ "Untypeable: " ++ show op ++ " " ++ show f ++ " " ++ show x
        (_, _, _) -> throwError "Untypeable"
typeof (Seq e1 e2) = do
    _ <- typeof e1 -- check and then throw away
    typeof e2
typeof (IfThenElse b e1 e2) = do
    b' <- typeof b
    if b' /= TyBool
       then throwError "guard is not a boolean"
       else do
           e1' <- typeof e1
           e2' <- typeof e2
           if e1' == e2' 
              then return e1'
              else throwError "arms of conditional do not match"
typeof err = throwError $ "This is either not (yet) typeable :( or you've made a mistake: " ++ show err
