-- | a mixture of two tutorials. Much simpler than rolling my own de Bruijn implementation
-- https://byorgey.wordpress.com/2011/03/28/binders-unbound/
-- https://github.com/lambdageek/unbound-generics

module Eval (eval) where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad
import Control.Arrow ((+++))
import Control.Monad.Trans.Maybe
import Control.Applicative ((<|>))

done :: MonadPlus m => m a
done = mzero

step :: Term -> MaybeT FreshM Term
step (Var _) = done
step (Lam _) = done
step (App (Lam b) t2) = do
    (x, t1) <- unbind b
    return $ subst x t2 t1
step (App t1 t2) = 
        App <$> step t1 <*> pure t2
    <|> App <$> pure t1 <*> step t2

tc :: (Monad m) => (a -> MaybeT m a) -> (a -> m a)
tc f a = do
    ma' <- runMaybeT (f a)
    case ma' of
         Just a' -> tc f a'
         Nothing -> return a

eval :: Term -> Term
eval x = runFreshM (tc step x)
