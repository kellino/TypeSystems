module Pretty (hoistError) where

--import Unbound.Generics.LocallyNameless
import Syntax
import Text.PrettyPrint 

hoistError :: (Show a) => Either a Term -> String
hoistError t = 
    case t of
         Left err -> show err
         Right res -> ppterm res

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Term where
    ppr _ (Var x) = text $ show x
    ppr _ (Lam f) = text $ show f
    ppr p (App t1 t2) = parensIf (p > 0) $ ppr p t1 <+> ppr (p+1) t2

ppterm :: Term -> String
ppterm = render . ppr 0 
