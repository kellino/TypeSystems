{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type Type = Term
type TName = Name Term

data Decl = Def TName Term

newtype Annot = Annot (Maybe Term) deriving (Show, Generic, Typeable)

data Ty =
      TyBool
    | TyArr Ty Ty
    | TyUnit
    | TyNat
    | TyFloat
    | TyString
    | TyRecord [Ty]
    | TyVariant String
        deriving (Eq, Show, Generic, Typeable)

data Term =
            Type Ty
          | TmTrue
          | TmFalse
          | TmUnit
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmFloat Double
          | TmTimesFloat Term Term
          | TmProjection Term String
          | TmString String
          | TmRecord [(String, Term)]
          | TmVariant String [(String, Term)]
          | Var TName
          | Lam (Bind (TName, Embed Annot) Term)
          | Let (Bind (TName, Embed Term) Term)
          | App Term Term
          | If Term Term Term 
          | Fix Term
          deriving (Show, Generic, Typeable)

instance Alpha Term
instance Alpha Ty
instance Alpha Annot

instance Subst Term Annot 

instance Subst Term Ty 

instance Subst Term Term where
    isvar (Var x) = Just (SubstName x)
    isvar _       = Nothing
