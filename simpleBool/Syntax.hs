{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type Error = String

data Ty = 
        TyBool
      | TyArr Ty Ty
        deriving (Show, Generic)

data Term = 
      TmTrue
    | TmFalse
    | Var (Name Term)
    | App Term Term
    | Abs (Bind (Name Term) Term)
    | If Term Term Term
          deriving (Show, Generic, Typeable)

instance Alpha Term

instance Subst Term Term where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing

