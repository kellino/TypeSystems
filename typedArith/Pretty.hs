module Pretty (ppterm, pptype) where

import Syntax

import Text.PrettyPrint (Doc, (<+>), text, render, parens)

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Term where
    ppr _ TmZero = text "0"
    ppr _ TmTrue = text "true"
    ppr _ TmFalse = text "false"
    ppr p (TmSucc a) = parensIf (p > 0) $ text "S" <+> ppr (p+1) a
    ppr p (TmPred a) = parensIf (p > 0) $ text "P" <+> ppr (p+1) a
    ppr p (TmIsZero a) = parensIf (p > 0) $ text "0?" <+> ppr (p+1) a
    ppr p (TmIf b t1 t2) = 
            text "if" <+> ppr p b
        <+> text "then" <+> ppr p t1
        <+> text "else" <+> ppr p t2

instance Pretty Type where
    ppr _ TyNat = text "ℕ"
    ppr _ TyBool = text "Bool"

ppterm :: Term -> String
ppterm = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0
