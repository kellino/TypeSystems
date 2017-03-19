module Intrinsic where

import Syntax
import Lattice

import Control.Monad.Except
import Unbound.Generics.LocallyNameless

type Eval a = ExceptT Doc FreshM a

runEval :: Term -> Either Doc Term
runEval = runFreshM . runExceptT . eval

eval :: Term -> Eval Term
eval = undefined
