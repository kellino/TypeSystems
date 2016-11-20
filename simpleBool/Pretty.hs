{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (ppr, pp) where

import Syntax
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import Unbound.Generics.LocallyNameless 

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Ty where
    ppr _ TyBool = dullred $ text "𝔹"
    ppr _ (TyArr a b) = ppr 0 a <> text "→" <> ppr 0 b

instance Pretty Term where
    ppr _ TmTrue = dullyellow $ text "true"
    ppr _ TmFalse = dullyellow $ text "false"
    ppr _ (Var x) = text (name2String x) <> space
    ppr _ (Abs x _) = text $ show x
    ppr p (App t1 t2) = parensIf (p > 0) $ ppr p t1 <> ppr (p+1) t2
    ppr p (If b t1 t2) = 
            text "if " <> ppr p b
        <> text "then " <> ppr p t1
        <> text "else " <> ppr p t2

instance Pretty Error where
    ppr _ = text . show

-- | helper function to pretty print the original unparsed text
pp :: String -> String
pp = map (\x -> if x == '\\' then 'λ' else x)
