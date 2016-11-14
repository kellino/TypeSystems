module Syntax where

data Ty = 
        TyBool
      | TyArr Ty Ty
        deriving Show

data Term = 
      TmTrue
    | TmFalse
    | Var String
    | App Term Term
    | Abs (String, Ty) Term 
    | If Term Term Term
          deriving Show
