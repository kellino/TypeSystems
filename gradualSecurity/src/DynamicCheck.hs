module DynamicCheck (evalDef, runDynamic) where

import Syntax
import Lattice

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import qualified Data.Map as M

evalDef :: TermEnv -> Binding -> TermEnv
evalDef env (nm, tm) =
    case runDynamic env nm tm of
         Left err -> env
         Right (_, termenv') -> termenv'

type Eval a = ExceptT TypeError FreshM a

data Closure = Closure String Term TermEnv

runDynamic :: TermEnv -> String -> Term -> Either TypeError (Term, TermEnv)
runDynamic env nm tm = 
    case runFreshM $ runExceptT (eval env tm) of
        Left err -> Left err
        Right ev -> Right (ev, M.insert nm ev env)

eval :: TermEnv -> Term -> Eval Term
eval env expr = 
    case expr of
         Val b l -> return $ Val b l

         Var n -> do
             let Just v = M.lookup (name2String n) env
             return v

         oper@(Op op b1 b2) -> do
             check env oper
             b1' <- eval env b1
             b2' <- eval env b2
             case op of
                  And -> b1' `bAnd` b2'
                  Or  -> b1' `bOr` b2'
                  Implies -> b1' `bImplies` b2'

         lam@Lam{} -> return lam

         App (Lam bnd) t2 -> do
             (((n, nl), _), body) <- unbind bnd
             args <- eval env t2
             let nenv = M.insert (name2String n) args env
             eval nenv t2

         app@(App t1 t2) -> do
             check env app
             t1' <- eval env t1
             eval env (App t1' t2)

         cond@(IfThenElse b tr fl) -> do
             check env cond
             b' <- eval env b
             case b' of
                 (Val TmTrue _) -> eval env tr
                 (Val TmFalse _) -> eval env fl
                 x -> throwError $ Undefined $ "expecting a boolean but got " ++ show x

         x -> throwError $ Undefined $ "not written yet: " ++ show x

-- helper functions for boolean operators
bAnd :: Term -> Term -> Eval Term
bAnd (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))
bAnd (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))
bAnd _ _ = throwError $ Undefined "undefined"

bOr :: Term -> Term -> Eval Term
bOr (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))
bOr (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))
bOr _ _ = throwError $ Undefined"undefined"

bImplies :: Term -> Term -> Eval Term
bImplies (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))
bImplies (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))
bImplies _ _ = throwError $ Undefined "undefined"

check :: TermEnv -> Term -> Eval Term
check env tm = 
    case tm of
         app@(App t1 (App t2 a)) -> return app
         app@App{} -> return app
         IfThenElse b a1 a2 -> do
             b' <- eval env b
             a1' <- eval env a1
             a2' <- eval env a2
             throwError $ Undefined ""
         x -> throwError $ Undefined $ show x


dynamicJoin :: GType -> GType -> Eval GType
dynamicJoin = undefined
