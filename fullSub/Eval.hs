module Eval where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except

type Eval a = ExceptT String FreshM a

runEval :: Term -> Either String Term
runEval = runFreshM . runExceptT . eval

eval :: Term -> Eval Term
eval TmTrue = return TmTrue
eval TmFalse = return TmFalse
eval TmZero = return TmZero
eval (TmIsZero TmZero) = return TmTrue
eval (TmIsZero _) = return TmFalse
eval f@TmFloat{} = return f
eval (TmFloatTimes (TmFloat x) (TmFloat y)) = return $ TmFloat (x * y)
eval x = throwError "not yet implemented"
