{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type TName = Name Term

data Ty = 
          TyBool
        | TyArr Ty Ty
        | TyNat
        | TyTop
        | TyBot
        | TyRecord [(String, Ty)]
        deriving (Show, Eq, Generic, Typeable)

data Term =
        TmVar TName
      | Type Ty
      | TmZero
      | TmIsZero Term
      | TmSucc Term
      | TmPred Term
      | TmTrue
      | TmFalse
      | TmError
      | TmRecord [(String, Term)]
      | TmProj Term String
      | TmAscription String Ty
      | TmAbs (Bind (TName, Embed [String]) Term)
      -- | TmAbs (Bind (TName, Embed Term) Term)
      | TmApp Term Term
      | TmIf Term Term Term
          deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Ty
--instance Alpha (Map.Map String Term)

instance Subst Term Term where
    isvar (TmVar x) = Just (SubstName x)
    isvar _         = Nothing

instance Subst Term Ty
--instance Subst Term (Map.Map String Term)
