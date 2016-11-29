{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax
import PrettyPrint
import Text.PrettyPrint.ANSI.Leijen

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.Identity

type TypeM = FreshMT (ExceptT Doc Identity)

runTypeOf :: Term -> Either Doc Ty
runTypeOf = runIdentity . runExceptT . runFreshMT . typeof

typeof :: Term -> TypeM Ty
typeof (TmVar _) = return TyBool -- placeholder
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmNumber{} = return TyNum
typeof (TmAbs bnd) = do
    ((_, unembed -> Annot t), body) <- unbind bnd
    b' <- typeof body
    case t of
         Nothing -> throwError $ display "missing annotation"
         Just t' -> 
            case t' of
              (Type (TyArr l r)) -> 
                  if b' == r
                     then return (TyArr l r)
                     else throwError $ display "annotation mismatch: the return type is " <> underline (display b') <> display " but the annotation says " <> underline (display r)
              (Type x) -> if x == b' then return b' else throwError $ display "annotation mismatch: expecting " <> underline (display x) <> display " but got " <> underline (display b')
              _ -> throwError $ display "invalid lambda type"
typeof TmError = return TyBot
typeof (TmIf b t1 t2) = do
    b' <- typeof b
    case b' of
         TyBool -> evalBool t1 t2
         TyBot  -> evalBool t1 t2
         _ -> throwError $ display "guard of conditional is not a boolean"
typeof (TmApp e1 e2) = do
    e1' <- typeof e1
    e2' <- typeof e2
    case e1' of
        (TyArr l r) -> if e2' == l then return r else throwError $ display "arg mismatch: " <> display e1' <> display " & " <> display e2'
        _ -> throwError $ display "expecting an arrow type"
typeof (TmTry t1 t2) = do
    t1' <- typeof t1
    t2' <- typeof t2
    case (t1', t2') of
        (TyBot, ty) -> return ty
        (ty, TyBot) -> return ty
        (x, y) -> if x == y then return y else throwError $ display "the left arm of try is " <> display x <> display " while the right is " <> display y
typeof x = throwError $ display x

------------------------------------
-- HELPER FUNCTIONS FOR SUBTYPING --
------------------------------------

evalBool :: Term -> Term -> TypeM Ty
evalBool t1 t2 = do
    t1' <- typeof t1
    t2' <- typeof t2
    case (t1', t2') of
        (TyBot, x) -> return x
        (x, TyBot) -> return x
        (x, y) -> if x == y 
               then return y 
               else throwError $ display "arms of conditional are of different types"

evalApp :: Ty -> Ty -> TypeM Ty
evalApp ty1 ty2 = undefined
