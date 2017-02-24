{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty where

import Syntax

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T

class Display a where
    display :: a -> Doc

instance Display T.Text where
    display = text . show

instance Display String where
    display = text

instance Display Expr where
    display (Skip l) = text "skip : " <> display l <> " Cmd"
    display (BoolExpr TmTrue l) = text "true : " <> display l
    display (BoolExpr TmFalse l) = text "false : " <> display l
    display (Num n l) = text $ show n ++ " : Int " ++ show l
    display (Assign (Var x l) y) = "var "  <> text x <> text " = " <> display y <> text " : " <> display l <> text " Cmd"
    display (Var x l) = text "var " <> display x <> display l
    display x = text $ show x

instance Display Label where
    display Low = text "Low"
    display Medium = text "Medium"
    display High = text "High"

instance Display Ty where
    display TyBool = green $ text "Bool"
    display TyNum = blue $ text "Int"
    display TyUnit = red $ text "()"
    display TyLoc = dullyellow $ text "Location"
