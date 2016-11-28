{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module TypeCheck where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.Identity

type TypeM = FreshMT (ExceptT String Identity)

runTypeOf :: Term -> Either String Ty
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
         Nothing -> throwError "missing annotation"
         Just t' -> 
            case t' of
              (Type (TyArr l r)) -> 
                  if b' == r
                     then return l
                     else throwError "annotation mismatch"
              (Type x) -> if x == b' then return b' else throwError "annotation mismatch"
              _ -> throwError "big error!"
typeof TmError = return TyBot
typeof (TmIf b t1 t2) = do
    b' <- typeof b
    case b' of
         TyBool -> evalBool t1 t2
         TyBot  -> evalBool t1 t2
         _ -> throwError "guard of conditional is not a boolean"
typeof (TmApp e1@TmAbs{} e2) = do
    e1' <- typeof e1
    e2' <- typeof e2
    if e1' == e2' 
       then return e2'
       else throwError "type mismatch"
typeof x = throwError $ show x

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
               else throwError "arms of conditional are of different types"
