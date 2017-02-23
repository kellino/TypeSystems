module Syntax where

type Name = String

data BoolExpr =
    TmTrue | TmFalse
    deriving (Show, Eq)

data Op = 
        Add 
      | Sub
      | Equal 
      | LessThan 
      | LessThanEq 
      deriving (Show, Eq)

data Expr =
        Var Name 
      | Num Integer
      {-| Add Expr Expr-}
      {-| Sub Expr Expr-}
      | BoolExpr BoolExpr
      | Skip  -- does nothing
      | Assign Expr Expr
      | App Expr Expr
      | IfThenElse Expr Expr Expr
      | While Expr Expr
      | Op Op Expr Expr
        deriving (Show, Eq)

