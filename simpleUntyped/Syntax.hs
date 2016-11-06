{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data Term =
    Var (Name Term)
  | App Term Term
  | Lam (Bind (Name Term) Term)
    deriving (Show, Generic, Typeable)

instance Alpha Term

instance Subst Term Term where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing
