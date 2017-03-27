{-# LANGUAGE ViewPatterns #-}

module StaticCheck where

import Syntax
import Lattice
import Gamma

import Unbound.Generics.LocallyNameless
import Control.Monad.Except hiding (join)
import Control.Monad.Reader hiding (join)
import Control.Monad.Identity hiding (join)
import qualified Data.Map as M

type StaticType = FreshMT (ReaderT Ctx (ExceptT TypeError Identity))

runOverFile :: Ctx -> [(String, Term)] -> Either TypeError Ctx
runOverFile env [] = Right env
runOverFile env (binding@(nm, _):xs) = do
    let new = runStatic env binding
    case new of
         Left err -> Left err
         Right gty -> runOverFile (extendGamma env (nm, gty)) xs

inEnv :: (String, GType) -> StaticType a -> StaticType a
inEnv (nm, gt) m = do
    let scope e = remove e nm `extendGamma` (nm, gt)
    local scope m

lookupVar :: String -> StaticType GType
lookupVar x = do
    st <- ask
    let (Gamma gm) = gamma st
    case M.lookup x gm of
         Nothing -> throwError $ NotInScope $ show gm
         Just e -> return e

runStatic :: Ctx -> Binding -> Either TypeError GType
runStatic ctx (_, t) =  runIdentity . runExceptT . runReaderT (runFreshMT . checkStatic $ t) $ ctx

checkStatic :: Term -> StaticType GType
checkStatic expr = 
    case expr of
         Val _ l -> return $ TyBool l

         Var n -> do
             let n' = name2String n
             lookupVar n'

         Op _ b1 b2 -> do
             b1' <- checkStatic b1
             b2' <- checkStatic b2
             b1' `gradJoin` b2'

         Lam bnd -> do
             (((n, nl), unembed -> Annot (Just l)), body) <- unbind bnd
             b' <- inEnv (name2String n, TyBool nl) (checkStatic body)
             return $ TyArr (TyBool nl) b' l

         App t1 t2 -> do
             t1' <- checkStatic t1
             t2' <- checkStatic t2
             case t1' of
                 (TyArr s1 s2 l) -> 
                     if t2' `isGradSubTypeOf` s1
                        then return $ TyBool (getLabel s2 \/ l)
                        else throwError $ ImplicitFlow s1 t2'
                 _ -> throwError $ Undefined $ "The first expression should be a function type, but is " ++ show t1'

         IfThenElse b t1 t2 -> do
             b' <- checkStatic b
             case b' of
                  (TyBool l) -> do
                      a1' <- checkStatic t1
                      a2' <- checkStatic t2
                      arms@(TyBool l') <- gradJoin a1' a2'
                      if l `flowsTo` l'
                         then gradJoin b' arms
                         else throwError $ ImplicitFlow b' arms
                  _ -> throwError $ Undefined "arms of conditional must be a boolean expression"

         _ -> throwError $ Undefined "not written yet"

gradJoin :: GType -> GType -> StaticType GType
gradJoin (TyBool l1) (TyBool l2) = return $ TyBool (l1 \/ l2)
gradJoin (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    t1 <- gradMeet s11 s21 
    t2 <- gradJoin s12 s22
    return $ TyArr t1 t2 (l \/ l')
gradJoin t1 t2 = throwError $ ImplicitFlow t1 t2

gradMeet :: GType -> GType -> StaticType GType
gradMeet (TyBool l1) (TyBool l2) = return $ TyBool (l1 /\ l2)
gradMeet (TyArr s11 s12 l) (TyArr s21 s22 l') = do
    t1 <- gradJoin s11 s21
    t2 <- gradMeet s12 s22
    return $ TyArr t1 t2 (l /\ l')
gradMeet t1 t2 = throwError $ ImplicitFlow t1 t2

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
