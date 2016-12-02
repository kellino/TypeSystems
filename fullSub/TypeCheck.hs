module TypeCheck where

import Syntax

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map

isNumeric :: Term -> Bool
isNumeric TmZero     = True
isNumeric (TmSucc t) = isNumeric t
isNumeric (TmPred t) = isNumeric t
isNumeric _          = False

type TypeM = FreshMT (ExceptT String Identity)

runTypeOf :: Term -> Either String Ty
runTypeOf = runIdentity . runExceptT . runFreshMT . typeof

typeof :: Term -> TypeM Ty
typeof TmError = return TyBot
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmZero = return TyNat
typeof (TmRecord flds) = do
    tys <- mapM (typeof . snd) flds
    return $ TyRecord $ zip (map fst flds) tys
typeof (TmProj (TmRecord rec) f) = do
    let found = filter ((== f) . fst) rec 
    if null found 
       then throwError "field does not exist"
       else typeof . snd . head $ found
typeof (TmSucc t) 
    | isNumeric t = return TyNat
    | otherwise = throwError "argument to succ is not a number"
typeof TmAbs{} = return TyTop
typeof (TmIf b t1 t2) = do
    b' <- typeof b
    case b' of
         TyBool -> do
             t1' <- typeof t1
             t2' <- typeof t2
             if t2' == TyBot 
                then return t1'
                else if t1' `isSubTypeOf` t2' 
                        then return t2' 
                        else throwError "arms of conditional are of different types"
         _ -> throwError "guard of conditional is not a boolean"
typeof (TmApp e1 e2) = do
    e1' <- typeof e1
    e2' <- typeof e2
    case (e1', e2') of
        (TyRecord r1, TyRecord r2) -> if e1' `isSubTypeOf` e2' then return e2' else throwError "grr"
        (TyTop, _) -> return TyTop
typeof x = throwError $ show x

isSubTypeOf :: Ty -> Ty -> Bool
isSubTypeOf _ TyTop = True
isSubTypeOf TyBot _ = True
isSubTypeOf (TyArr s1 s2) (TyArr t1 t2) = isSubTypeOf t1 s1 && isSubTypeOf s2 t2
isSubTypeOf r1@TyRecord{} r2@TyRecord{} = recordCheck r1 r2
isSubTypeOf x y
    | x == y     = True
    | otherwise = False

recordCheck :: Ty -> Ty -> Bool
recordCheck (TyRecord r1) (TyRecord r2) = r2' `Map.isSubmapOf` r1'
    where r1' = Map.fromList r1
          r2' = Map.fromList r2
