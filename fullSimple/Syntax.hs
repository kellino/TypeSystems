{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type Error = String

data Ty = 
        TyBool
      | TyArr Ty Ty
      | TyNat
      | TyUnit
      | TyString
      | TyFloat
      | TyRecord [Ty]
        deriving (Show, Eq, Generic)

data Term = 
      TmTrue
    | TmFalse
    | TmZero
    | TmUnit
    | TmString String
    | TmFloat Double
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    | TmRecord [(String, Term)] String
    | Var (Name Term)
    | App Term Term
    | Abs (Bind (Name Term) Term) Ty
    | If Term Term Term
    | Let String Term Term
          deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Ty

instance Subst Term Ty where
    isvar _       = Nothing

instance Subst Term Term where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing

