{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}

module Syntax where

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type EName = Name Expr

data Phrase =
      Ex Expr
    | Co Command
      deriving (Show, Generic, Typeable)

data Expr =
        Var (Name Expr)
      | Num Integer
      | Add Expr Expr
      | Sub Expr Expr
      | Equal Expr Expr
      | Lt Expr Expr
        deriving (Show, Generic, Typeable)

data Command =
       Assign Expr Expr
     | App Command Command
     | IfThenElse Expr Command Command
     | While Expr Command
     | Let (Bind (EName, Embed Expr) Command)
       deriving (Show, Generic, Typeable)

instance Alpha Expr
instance Alpha Command
instance Alpha Phrase

instance Subst Expr Expr where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing
