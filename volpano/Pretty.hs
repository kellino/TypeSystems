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
    display (Skip l) = text $ "skip : " ++ show l ++ " Cmd"
    display (BoolExpr TmTrue l) = text $ "true : Bool " ++ show l
    display (BoolExpr TmFalse l) = text $ "false : Bool " ++ show l
    display (Num n l) = text $ show n ++ " : Int " ++ show l
    display (Assign (Var x l) y) = text $ "var "  ++ x ++ " = " ++ show y ++ " : " ++ show l ++ " Cmd"
    display (Var x l) = text $ "var " ++ x ++ " : " ++ show l
    display x = text $ show x

instance Display Ty where
    display TyBool = green $ text "Bool"
    display TyNum = blue $ text "Num"
    display TyUnit = red $ text "()"
    display TyLoc = dullyellow $ text "Location"
