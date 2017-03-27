{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Lattice

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable
import Control.Monad.State.Strict
import qualified Data.Map as M
import Control.Exception hiding (TypeError)

-----------------------
-- Environment Types --
-----------------------

type StaticOut a = StateT Ctx IO a

type TermEnv = M.Map String Term 

newtype Gamma = Gamma (M.Map String GType) deriving Monoid

instance Show Gamma where
    show (Gamma g) = show g

data Ctx = Ctx { termenv :: TermEnv, gamma :: Gamma } deriving Show

-----------------
-- Type Errors --
-----------------

data TypeError
    = ImplicitFlow GType GType
    | TransitiveMismatch GType GType GType
    | NotInScope String
    | UnboundVariable String
    | Undefined String
    deriving Typeable

instance Show TypeError where
    show (ImplicitFlow g1 g2) = "ImplicitFlow between " ++ show g1 ++ " and " ++ show g2
    show (TransitiveMismatch g1 g2 g3) = "TransitiveMismatch"
    show (NotInScope x) = x ++ " is not in scope"
    show (UnboundVariable x) = show x ++ " is not bound"
    show (Undefined s) = s

instance Exception TypeError

---------------------
-- Concrete Syntax --
---------------------

type Binding = (String, Term)

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
    | Lam (Bind ((TName, GLabel), Embed Annot) Term)
    | App Term Term
    | IfThenElse Term Term Term
    | Op BinOp Term Term 
    | Ascription Term GLabel
      deriving (Show, Generic, Typeable)

newtype Annot = Annot (Maybe GLabel) deriving (Show, Generic, Typeable)

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
