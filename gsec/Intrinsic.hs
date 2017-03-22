module Intrinsic where

import Syntax
import Lattice
import Gamma

import Control.Monad.Except
import Unbound.Generics.LocallyNameless

type Dynamic a = ExceptT String FreshM a

runDynamic :: Gamma -> String -> Term -> (GType, Gamma)
runDynamic env nm tm =
    let res = runFreshM . runExceptT $ eval env tm 
        in case res of
                Left err -> error ""
                Right r -> (r, extend env (nm, r))

eval :: Gamma -> Term -> Dynamic GType
eval env tm = case tm of
    (Var x) -> do
        let nm = name2String x
        let f = lookupType nm env
        case f of
             Nothing -> throwError "not found in env"
             (Just r) -> return r
    (Val _ l) -> return $ TyBool l

consistentTrans = undefined 

----------------------------
-- Transitive Consistency --
----------------------------

transConsistency :: GType -> GType -> GType -> Dynamic GType
transConsistency = undefined

recMeet :: GType -> GType -> Dynamic GType
recMeet (TyBool l) (TyBool l') = do
    nl <- meet l l'
    return $ TyBool nl
recMeet (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    m1 <- recMeet s11 s21
    m2 <- recMeet s12 s22
    nl <- meet l l'
    return $ TyArr m1 m2 nl
recMeet s1 s2 = throwError $ "recursive meet undefined for " ++ show s1 ++ " " ++ show s2

meet :: GLabel -> GLabel -> Dynamic GLabel
meet l Any = return l
meet Any l = return l
meet l l' | l == l' = return l
          | otherwise = throwError $ "undefined for " ++ show l ++ " and " ++ show l'



-- helper functions for boolean operators
{-bAnd :: Term -> Term -> Dynamic Term-}
{-bAnd (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))-}
{-bAnd (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))-}
{-bAnd _ _ = throwError "undefined"-}

{-bOr :: Term -> Term -> Dynamic Term-}
{-bOr (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))-}
{-bOr (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))-}
{-bOr _ _ = throwError "undefined"-}

{-bImplies :: Term -> Term -> Dynamic Term-}
{-bImplies (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))-}
{-bImplies (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))-}
{-bImplies _ _ = throwError "undefined"-}
