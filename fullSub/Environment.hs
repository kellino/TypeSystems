module Environment where

import Syntax

import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

type TypeM a = FreshMT (ReaderT Env (StateT TypeEnv (Except String))) a

data Env = Env { ctx :: [(String, Ty)] }
         deriving Show

data TypeEnv = TypeEnv { types :: [(String, Ty)] } deriving Show

initEnv :: Env
initEnv = Env { ctx = [] }

fromTypeEnv :: String -> TypeM Ty
fromTypeEnv x = do
    env <- gets types
    case lookup x env of
         Nothing -> throwError $ show env
         Just r -> return r

getName :: Fresh m => Term -> m TName
getName (TmAbs bnd) = do
    ((x, _), _) <- unbind bnd
    return x
getName _ = error "" -- this can't happen

addToTypeEnv :: (String, Ty) -> TypeM TypeEnv
addToTypeEnv (s, ty) = do
    st <- gets types
    let st' = TypeEnv { types = (s, ty):st }
    put st'
    return st'

-- | ugly hack. Assumes de Bruijn indices don't go above 10
decrement :: TName -> String
decrement xs = 
    if isDigit (last xs') 
       then init xs' ++ edit (last xs')
       else xs'
    where xs' = show xs
          edit s
              | s == '1' = ""
              | otherwise = show $ (read [s] :: Int) - 1

filterAscrips :: [Either t Term] -> [Either t Term]
filterAscrips [] = []
filterAscrips (x:xs) =
    case x of 
        (Right TmAscription{}) -> x : filterAscrips xs
        _ -> filterAscrips xs

primitives :: [(String, Ty)]
primitives = [("Nat", TyNat), ("Bool", TyBool), ("String", TyString), ("Float", TyFloat)] 

addToContext :: [Either a Term] -> TypeEnv
addToContext [] = TypeEnv { types = primitives }
addToContext xs = TypeEnv { types = newCxt xs }

newCxt :: [Either a Term] -> [(String, Ty)]
newCxt [] = primitives 
newCxt (Left _:xs) = newCxt xs
newCxt (Right (TmAscription ty n) : xs) = (n, toType ty) : newCxt xs

toType :: [String] -> Ty
toType xs = foldl1 TyArr (new xs)
    where new = map tt 
          tt x = fromMaybe (error "") (lookup x primitives)
