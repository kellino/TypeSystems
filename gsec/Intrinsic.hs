module Intrinsic where

import Syntax
import Lattice

import Control.Monad.Except
import Unbound.Generics.LocallyNameless

type Eval a = ExceptT String FreshM a

runEval :: Term -> Either String Term
runEval = runFreshM . runExceptT . eval

eval :: Term -> Eval Term
eval v@Var{} = return v
eval v@Val{} = return v
eval (Op op b1 b2) = do
    b1' <- eval b1
    b2' <- eval b2
    case op of
         And -> b1' `bAnd` b2'
         Or -> b1' `bOr` b2'
         Implies -> b1' `bImplies` b2'
eval _ = throwError "undefined"


-- helper functions for boolean operators
bAnd :: Term -> Term -> Eval Term
bAnd (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))
bAnd (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))
bAnd _ _ = throwError "undefined"

bOr :: Term -> Term -> Eval Term
bOr (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))
bOr (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))
bOr _ _ = throwError "undefined"

bImplies :: Term -> Term -> Eval Term
bImplies (Val TmTrue l) (Val TmTrue l') = return (Val TmTrue (l \/ l'))
bImplies (Val _ l) (Val _ l') = return (Val TmFalse (l \/ l'))
bImplies _ _ = throwError "undefined"
