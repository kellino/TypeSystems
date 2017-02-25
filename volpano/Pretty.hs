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
    display (Skip l) = bold $ text "skip : " <> display TyUnit <> display l <> cmd
    display (BoolExpr TmTrue l) = text "true : " <> display TyBool <> display l
    display (BoolExpr TmFalse l) = text "false : " <> display TyBool <> display l
    display (Num n l) = display (show n) <> text " : " <> display TyNum <> display l
    display (Assign (Var x l) (Num n _)) = "var "  <> text x <> text " = " <> text (show n) <> text " : " <> display l <> cmd
    display (Var x l) = text "var " <> display x <> text " : " <> display l
    display x = text $ show x

cmd :: Doc
cmd = bold . red $ text " Cmd"

instance Display Label where
    display Low = ondullgreen $ text "Low"
    display Medium = ondullblue $ text "Medium"
    display High = ondullcyan $ text "High"

instance Display Ty where
    display TyBool = green $ text "Bool "
    display TyNum = blue $ text "Int "
    display TyUnit = red $ text "() "
    display TyLoc = dullyellow $ text "Location "
