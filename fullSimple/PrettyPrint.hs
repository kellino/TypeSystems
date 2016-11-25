{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint where

import Syntax
import TypeCheck
import Environment

import Data.Char
import Text.PrettyPrint.ANSI.Leijen

class Display a where
    display :: a -> Doc

instance Display String where
    display = text . pp

instance (Display b) => Display (Either String b) where
    display (Left a) = ppError a 
    display (Right b) = display b 

instance Display (Term, Ty) where
    display (a, b) = display a <> bold (text " : ") <> display b

instance Display Term where
    display (TmString str) = text str
    display TmTrue = text "true"
    display TmFalse = text "false"
    display TmUnit = text "()"
    display TmZero = text "0"
    display (TmFloat f) = text $ show f
    display (TmSucc t) = text $ show $ 1 + count t
    display (TmPred t) = text $ show $ count t
    display (TmRecord xs) = braces $ cat $ punctuate comma (map (display . runTypeOf emptyEnv . snd) xs)
    display x = text $ show x

instance Display Ty where
    display TyBool = green $ text "Bool"
    display TyUnit = dullcyan $ text "()"
    display TyNat = dullyellow $ text "Nat"
    display (TyArr a b) = display a <> blue (text "→") <> display b
    display TyFloat{} = yellow $ text "Float"
    display TyString{} = magenta $ text "String"
    display TyRecord{} = cyan $ text "Record"
    display (TyVariant n) = dullred $ text n

count :: Term -> Int
count TmZero = 0
count (TmSucc t) = 1 + count t

pp :: String -> String
pp = map (\x -> if x == '\\' then 'λ' else x) . takeWhile (/= '#')

ppError :: String -> Doc
ppError s = red (text "Error") <> text " : " <> edit (words s)
    where edit = foldr (\x y-> alter x <> space <> y) (text "")
          alter t@(n:_) 
              | isUpper n = underline (text t)
              | t == "not" = bold $ text t
              | otherwise = text t
