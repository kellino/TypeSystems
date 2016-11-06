module Eval where

import Syntax
import Control.Monad.Identity
import Control.Monad.Except

isNumerical :: Term -> Bool
isNumerical TmZero      = True
isNumerical (TmSucc t1) = isNumerical t1
isNumerical _           = False

isVal :: Term -> Bool
isVal TmTrue  = True
isVal TmFalse = True
isVal t | isNumerical t = True
isVal _       = False

hasType :: Type -> Type -> Bool
x `hasType` y = x == y

type Eval a = ExceptT Error Identity a

runEval :: Term -> Either Error Term
runEval = runIdentity . runExceptT . eval 

eval :: Term -> Eval Term
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval TmZero = return TmZero
eval (TmIf TmTrue t1 _) = return t1
eval (TmIf TmFalse _ t2) = return t2
eval (TmIf b t1 t2) = do
    b' <- eval b
    return $ TmIf b' t1 t2
eval (TmSucc t1) = do
    t1' <- eval t1
    return $ TmSucc t1'
eval (TmPred TmZero) = return TmZero
eval (TmPred t1) = do
    t1' <- eval t1
    return $ TmPred t1'
eval (TmIsZero TmZero) = return TmTrue
eval (TmIsZero (TmSucc nv)) | isNumerical nv = return TmFalse
eval (TmIsZero t1) = do
    t1' <- eval t1
    return $ TmIsZero t1'

runTypeOf :: Term -> Either Error Type
runTypeOf = runIdentity . runExceptT . typeof

typeof :: Term -> Eval Type
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof (TmIf b t1 t2) = do
    b' <- typeof b
    if b' `hasType` TyBool
       then do
           t1' <- typeof t1
           t2' <- typeof t2
           if t1' == t2' 
              then return t2'
              else throwError "\ESC[1mbranches of condition are different types\ESC[0m"
       else throwError "\ESC[1mguard in conditional is not a boolean\ESC[0m"
typeof TmZero = return TyNat
typeof (TmSucc t1) = do
    t1' <- typeof t1
    if t1' `hasType` TyNat
       then return TyNat
       else throwError "\ESC[1margument of succ is not a number\ESC[0m"
typeof (TmPred t1) = do
    t1' <- typeof t1
    if t1'`hasType` TyNat
       then return TyNat
       else throwError "\ESC[1margument of pred is not a number\ESC[0m"
typeof (TmIsZero t1) = do
    t1' <- typeof t1
    if t1' `hasType` TyNat
       then return TyBool
       else throwError "\ESC[1margument of iszero is not a number\ESC[0m"

typeAndEval :: Term -> Either Error (Term, Type)
typeAndEval t = do
    ty <- runTypeOf t
    term <- runEval t
    return (term, ty)
