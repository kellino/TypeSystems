{-# LANGUAGE DeriveGeneric, DeriveDataTypeable,
             MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type TName = Name Term

data Ty
    = TyNat
    | TyBool
    | TyTop
    | TyBot
    | TyUnit
    | TyArr Ty Ty
  deriving (Show, Eq, Generic, Typeable)

data Term
    = TmVar TName
    | TmTrue
    | TmFalse
    | TmSucc Term
    | TmPred Term
    | TmZero
    | TmIsZero Term
    | TmError
    | TmAscription [String] String
    | TmApp Term Term
    | TmAbs (Bind (TName, Embed [String]) Term)
    | TmIf Term Term Term
  deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Ty

instance Subst Term Term where
    isvar (TmVar x) = Just (SubstName x)
    isvar _         = Nothing

instance Subst Term Ty
