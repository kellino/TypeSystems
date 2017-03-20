{-# LANGUAGE ViewPatterns #-}

module Static where

import Lattice
import Syntax
import Gamma

import Unbound.Generics.LocallyNameless
import Control.Monad.Except hiding (join)
import Control.Monad.State hiding (join)
import Control.Monad.Identity hiding (join)
import qualified Data.Map as M

data Ctx = Ctx { termctx :: TermEnv, gammactx :: Gamma } deriving Show

emptyContext :: Ctx
emptyContext = Ctx { termctx = M.empty, gammactx = M.empty }

type StaticType = FreshMT (StateT Ctx (ExceptT String Identity))

runStatic :: Ctx -> Term -> Either String (GType, Ctx)
runStatic ctx t = runIdentity . runExceptT $ runStateT (runFreshMT $ typeOf t) ctx

typeOf :: Term -> StaticType GType
-- variables
typeOf (Var n) = do
    en <- get
    let nty = lookupType (name2String n) (gammactx en)
    return $ TyArr (TyBool L) (TyBool L) L
    --return nty
-- values
typeOf (Val _ t) = return $ TyBool t
-- boolean operations
typeOf (Op _ b1 b2) = do
    (TyBool l) <- typeOf b1
    (TyBool l') <- typeOf b2
    return $ TyBool (l \/ l')
-- lambda abstraction
typeOf (Lam bnd) = do
    ((n, unembed -> Annot l), body) <- unbind bnd
    b' <- typeOf body
    case l of
        Nothing -> return $ TyArr (TyBool Any) b' Any
        (Just l') -> return $ TyArr (TyBool l') b' (l' /\ getLabel b')
-- application    
typeOf (App t1 t2) = do
    t1' <- typeOf t1
    s2 <- typeOf t2
    case t1' of
        (TyArr s11 (TyBool l) l') -> 
            if s2 `isGradSubTypeOf` s11
               then return $ TyBool (l \/ l')
               else throwError $ show s2 ++ " is not a consistent subtype of " ++ show s11
        TyArr{} -> throwError "cannot return a function"
        _ -> throwError "first element must be a function"
-- control flow
typeOf (IfThenElse b a1 a2) = do
    b' <- typeOf b
    case b' of
         (TyBool l) -> do
             a1' <- typeOf a1
             a2' <- typeOf a2
             arms@(TyBool l') <- gradJoin a1' a2'
             if l `flowsTo` l'
                then gradJoin b' arms
                else throwError $ "flow between " ++ show l ++ " and " ++ show l' ++ " is not allowed."
         _ -> throwError "arms of conditional must be a boolean expression"
-- ascription 
typeOf (Ascription t l) = do
    t' <- typeOf t
    if getLabel t' `flowsTo` l
       then return $ setLabel t' l
       else throwError $ show t' ++ " is not a subtype of " ++ show l

gradJoin :: GType -> GType -> StaticType GType
gradJoin (TyBool l1) (TyBool l2) = return $ TyBool (l1 \/ l2)
gradJoin (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    t1 <- gradMeet s11 s21 
    t2 <- gradJoin s12 s22
    return $ TyArr t1 t2 (l \/ l')
gradJoin _ _ = throwError "undefined"

gradMeet :: GType -> GType -> StaticType GType
gradMeet (TyBool l1) (TyBool l2) = return $ TyBool (l1 /\ l2)
gradMeet (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    t1 <- gradJoin s11 s21
    t2 <- gradMeet s12 s22
    return $ TyArr t1 t2 (l /\ l')
gradMeet _ _ = throwError "undefined"

setLabel :: GType -> GLabel -> GType
setLabel (TyBool _) l' = TyBool l'
setLabel (TyArr x y _) l' = TyArr x y l'

getLabel :: GType -> GLabel
getLabel (TyBool l) = l
getLabel (TyArr _ _ l) = l

isGradSubTypeOf :: GType -> GType -> Bool
(TyBool l) `isGradSubTypeOf` (TyBool l') = l `flowsTo` l'
(TyArr s1 s2 l) `isGradSubTypeOf` (TyArr s1' s2' l') = s1' `isGradSubTypeOf` s1 && s2 `isGradSubTypeOf` s2' && l `flowsTo` l'
_ `isGradSubTypeOf` _ = undefined
