{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable

type TName = Name Term 

data Ty =
        TyBool
        | TyArr Ty Ty
        | TyTop
        | TyBot
        | TyVar
        | TyNum
          deriving (Eq, Show, Generic, Typeable)

data Term =
        Type Ty
      | TmVar TName
      | TmAbs (Bind (TName, Embed Annot) Term)
      | TmApp Term Term
      | TmFalse
      | TmTrue
      | TmNumber Double
      | TmIf Term Term Term
      | TmError
      | TmTry
        deriving (Show, Generic, Typeable)

newtype Annot = Annot (Maybe Term) deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Annot
instance Alpha Ty

instance Subst Term Ty
instance Subst Term Annot 

instance Subst Term Term where
    isvar (TmVar x) = Just (SubstName x)
    isvar _       = Nothing
