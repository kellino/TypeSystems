module Syntax where

data Type =
    TyBool
  | TyNat
          deriving (Show, Eq)

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
          deriving Show

type Error = String
