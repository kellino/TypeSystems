{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type TName = Name Term

data Ty = 
          TyBool
        | TyArr Ty Ty
        | TyNat
        deriving (Show, Eq, Generic, Typeable)

data Term =
        TmVar TName
      | TmZero
      | TmIsZero Term
      | TmSucc Term
      | TmPred Term
      | TmAbs (Bind (TName, Ty) Term)
      | TmApp Term Term
          deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Ty

instance Subst Term Term where
    isvar (TmVar x) = Just (SubstName x)
    isvar _         = Nothing

instance Subst Term Ty
