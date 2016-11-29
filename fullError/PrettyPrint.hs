{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PrettyPrint where

import Syntax

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T

class Display a where
    display :: a -> Doc

instance Display T.Text where
    display = text . show

instance Display String where
    display = text

instance Display (Either Doc (Term, Ty)) where
    display (Left err) = red (text "Error: ") <> err <> hardline
    display (Right res) = display res <> hardline

instance Display (Term, Ty) where
    display (term, ty) = display term  <> bold (text " : ") <> display ty

instance Display Term where
    display TmTrue = text "true"
    display TmFalse = text "false"
    display TmError = text "error"
    display (TmNumber n) = text $ show n
    display x = text $ show x

instance Display Ty where
    display TyBool = green $ text "Bool"
    display TyNum = blue $ text "Num"
    display (TyArr l r) = display l <> dullyellow (text "→") <> display r
