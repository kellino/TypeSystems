module Syntax where

type Name = String

--data CV = Cmd | Vr deriving (Show, Eq)

--------------------
-- Simple Lattice --
--------------------

-- thanks to Steve Zdancewic to this simple class

class Ord a => Lattice a where
    top :: a 
    bottom :: a
    join :: a -> a -> a
    meet :: a -> a -> a
    eq :: a -> a -> Bool

data Label = Low | Medium | High 
           deriving (Show, Eq, Ord)

instance Lattice Label where
    top = High
    bottom = Low
    join x y = if x `eq` y then y else x
    meet x y = if x `eq` y then x else y
    eq Low _ = True
    eq Medium Low = False
    eq Medium _ = True
    eq High High = True
    eq High _ = False

----------------
-- Type Types --
----------------

data Ty = TyNum 
        | TyBool 
        | TyLoc     -- for (null) var declarations
        | TyUnit
        deriving (Show, Eq)

data BoolExpr =
    TmTrue | TmFalse
    deriving (Show, Eq)

data Op = 
        Add 
      | Sub
      | Equal 
      | LessThan 
      | LessThanEq 
      deriving Eq

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Equal = "≡"
    show LessThan = "<"
    show LessThanEq = "≤"

data Expr =
        Var Name Label
      | Num Integer Label
      | BoolExpr BoolExpr Label
      | Skip Label 
      | Assign Expr Expr 
      | Seq Expr Expr
      | App Expr Expr
      | IfThenElse Expr Expr Expr
      | While Expr Expr
      | Op Op Expr Expr
      | Lattice String
        deriving (Show, Eq)

-------------
-- Aliases --
-------------

type Error = String
type UnTypeable = String
