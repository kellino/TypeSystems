{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type TName = Name Term

data Ty 
    = TyInt
    | TyBool
    | TyArrow Ty Ty
    | TyAny
    deriving (Show, Eq, Generic, Typeable)

data Term 
    = TmTrue
    | TmFalse
    | TmVar TName
    | TmInt Integer
    | TmSucc Integer
    | TmPred Integer
    | TmIsZero Integer
    | TmApp Term Term
    | TmAbs (Bind (TName, Embed [String]) Term)
    | TmLet (Bind (TName, Embed Term) Term)
    deriving (Show, Generic, Typeable)

instance Alpha Ty
instance Alpha Term

instance Subst Term Term where
    isvar (TmVar x) = Just (SubstName x)
    isvar _         = Nothing

instance Subst Term Ty
