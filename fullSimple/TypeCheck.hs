module TypeCheck where

import Syntax
import Control.Monad.Except
import Control.Monad.Identity

type Type a = ExceptT String Identity a

typeof :: Term -> Type Ty
typeof TmUnit = return TyUnit
typeof (TmIsZero _) = return TyBool
typeof (Var _) = return TyBool -- placeholder
typeof TmZero = return TyNat
typeof (TmSucc t) = do
    t' <- typeof t
    case t' of
         TyNat -> return TyNat
         err    -> throwError $ show err ++ " is not a number"
typeof (TmPred TmZero) = return TyNat
typeof (TmPred t) = typeof t
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof (Abs _ t) = return t
typeof (App t1 t2) = do
    t1' <- typeof t1
    t2' <- typeof t2
    case t1' of
         TyArr t11 t12 ->
             if t11 == t2' 
                then return t12
                else throwError "parameter type mismatch"
         _ -> throwError "arrow type expected"
typeof (If b t1 t2) = do
    b' <- typeof b
    case b' of
         TyBool -> do
             t1' <- typeof t1
             t2' <- typeof t2
             if t1' /= t2'
                then throwError "arms of conditional have different types"
                else return t1'
         _  -> throwError "guard of conditional is not a boolean"

runTypeOf :: Term -> Either String Ty
runTypeOf = runIdentity . runExceptT . typeof
