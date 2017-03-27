{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint where

import Syntax
import Lattice

import Text.Megaparsec
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T

class Display d where
    display :: d -> Doc

instance Display (ParseError Char Dec) where
    display x = text . show $ x

instance Display GType where
    display (TyBool l) = blue (text "Bool") <> display l
    display (TyArr a b l) = display a <> bold (green (text " →")) <> display l <> text " " <> display b

instance Display GLabel where
    display L = bold . red $ text "ₗ"
    display MA = bold . red $ text "ₘₐ"
    display MB = bold . red $ text "ₘᵦ"
    display H = bold . red $ text "ₕ"
    display Any = bold . red $ text "⋆"

instance Display Term where
    display (Val TmTrue l) = text "True" <> display l
    display (Val TmFalse l) = text "False" <> display l
    display Lam{} = bold . text $ "<<closure>>"
    display x = text $ show x

instance Display TypeError where
    display (ImplicitFlow g1 g2) = text "Implicit flow found between " <> display g1 <> text " and " <> display g2 <> hardline
    display (TransitiveMismatch g1 g2 g3) = text "Dynamic security error between " <> display g1 <> display g2 <> text " and " <> display g3 <> hardline
    display (Undefined s) = text s
    display x = text $ show x

{-data TypeError-}
    {-= ImplicitFlow GType GType-}
    {-| TransitiveMismatch GType GType GType-}
    {-| NotInScope String-}
    {-| UnboundVariable String-}
    {-| Undefined String-}
    {-deriving Typeable-}

-- pretty print static output
ppstatic :: (String, GType) -> Doc
ppstatic (nm, ty) = text nm <> text " : " <> display ty <> hardline
