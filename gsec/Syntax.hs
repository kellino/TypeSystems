{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses #-}

module Syntax where

import Lattice

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable

type TName = Name Term

data Boolean = TmTrue | TmFalse deriving (Eq, Show, Generic, Typeable)

data GType =
      TyBool GLabel
    | TyArr GType GType GLabel
      deriving (Eq, Show, Generic, Typeable)

data BinOp = And | Or | Implies deriving (Show, Generic, Typeable)

data Term =
      Var TName
    | Val Boolean GLabel
    | Lam (Bind (TName, Embed Annot) Term)
    | App Term Term
    | IfThenElse Term Term Term
    | Op BinOp Term Term 
    | Ascription Term GLabel
      deriving (Show, Generic, Typeable)

newtype Annot = Annot (Maybe Term) deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha BinOp
instance Alpha GType
instance Alpha Boolean
instance Alpha GLabel
instance Alpha Annot

instance Subst Term Term where
    isvar (Var x) = Just (SubstName x)
    isvar _       = Nothing

instance Subst Term Boolean 
instance Subst Term GType
instance Subst Term GLabel
instance Subst Term Annot
instance Subst Term BinOp
