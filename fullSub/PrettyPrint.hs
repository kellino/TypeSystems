{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint where

import Text.PrettyPrint.ANSI.Leijen
import TypeCheck
import Syntax

class Display d where
    display :: d -> Doc

instance Display (Either String (Term, Ty)) where
    display (Left err) = text "Error: " <> text err <> hardline
    display (Right (x, y)) = display x <> text " : " <> display y <> hardline

instance Display (Either String Term) where
    display (Left err) = text err <> hardline
    display (Right r) = display r <> hardline

instance Display (Either String Ty) where
    display (Left err) = text err <> hardline
    display (Right r) = display r <> hardline

instance Display Term where
    display TmTrue = text "true"
    display TmFalse = text "false"
    display TmZero = text "0"
    display s@TmSucc{} = text . show $ count s
    display p@TmPred{} = text . show $ count p
    display (TmRecord tys) = braces $ hcat $ punctuate comma (map (display . runTypeOf . snd) tys)
    display x = text . show $ x

instance Display Ty where
    display TyBool = text "Bool"
    display TyNat = text "Nat"
    display (TyArr t1 t2) = display t1 <> text "→" <> display t2
    display x = text . show $ x

count :: Term -> Int
count TmZero = 0
count (TmSucc t) = 1 + count t
count (TmPred t) = count t
