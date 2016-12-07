-- | borrows heavily from the pretty printer in pi-forall 
-- https://github.com/sweirich/pi-forall/blob/2014/version1/src/PrettyPrint.hs

{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, DefaultSignatures, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint where

import Syntax

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Unbound.Generics.LocallyNameless
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Set as S
import Control.Arrow ((***))

class Disp d where
    disp :: d -> Doc

    default disp :: (Display d) => d -> Doc
    disp = cleverDisp

cleverDisp :: (Display d) => d -> Doc
cleverDisp d = runReaderDispInfo (display d) initDI

instance Disp Term
instance Disp Ty

initDI :: DispInfo
initDI = DI S.empty

data DispInfo = DI { dispAvoid :: S.Set AnyName }

newtype ReaderDispInfo a = ReaderDispInfo (ReaderT DispInfo Identity a)
    deriving (Functor, Applicative, Monad, MonadReader DispInfo)

instance LFresh ReaderDispInfo where
    lfresh nm = do
        let s = name2String nm
        di <- ask
        return $ head (filter (\x -> AnyName x `S.notMember` dispAvoid di) (map (makeName s) [0..]))
    getAvoids = dispAvoid <$> ask
    avoid names = local upd 
        where upd di = di { dispAvoid = S.fromList names `S.union` dispAvoid di }

runReaderDispInfo :: ReaderDispInfo a -> DispInfo -> a
runReaderDispInfo (ReaderDispInfo comp) = runReader comp

type M a = ReaderDispInfo a

class (Alpha t) => Display t where
    display :: t -> M Doc

instance Disp (Either String Ty)

instance Disp String where
    disp = text

instance Display (Either String Ty) where
    display (Left err) = display err
    display (Right ty) = display ty

instance Display String where
    display = return . text

instance Display Term where
    display (TmVar x) = return $ (text . name2String) x
    display TmTrue = return $ text "true"
    display TmFalse = return $ text "false"
    display TmZero = return $ text "0"
    display (TmIsZero b) = do 
        b' <- display b
        return $ text "isZero? " <> b'
    display (TmSucc t) = return $ text . show $ 1 + count t
    display (TmPred t) = display t
    display (TmString str) = return $ text "\"" <> text str <> text "\""
    display (TmFloat f) = return $ double f
    display (TmFloatTimes f1 f2) = do
        f1' <- display f1
        f2' <- display f2
        return $ f1' <> text " * " <> f2'
    display a@TmAbs{} = do
        (binds, body) <- gatherBinders a
        return $ (text "λ" <> sep binds <> text ".") <> body
    display (TmAscription tys n) = do 
        let tys' = displayType tys
        return $ text n <> text " ascribed the type of " <> tys'
    display (TmRecord re) = do
        let tups = map (\(x, y) -> disp x <> text " = " <> disp y) re
        return $ braces $ hcat $ punctuate (text ", ") tups

instance Display Ty where
    display TyTop = return $ text "Top"
    display TyBool = return $ dullblue $ text "Bool"
    display TyNat = return $ red $ text "Nat"
    display TyUnit = return $ bold $ text "()"
    display TyString = return $ dullmagenta $ text "String"
    display TyBot = return $ text "Bot"
    display TyFloat = return $ dullyellow $ text "Float"
    display (TyArr l r) = do
        l' <- display l
        r' <- display r
        return $ l' <> bold (blue (text " → ")) <> r'
    display (TyRecord tys) = do 
        tys' <- mapM display tys
        return $ braces $ hcat $ punctuate (text ", ") tys'

remComments :: T.Text -> T.Text
remComments = T.strip . T.takeWhile (/= '#') 

count :: Term -> Int
count TmZero = 0
count (TmSucc t) = 1 + count t
count x = error $ show x

displayType :: [String] -> Doc
displayType xs = hcat $ punctuate (text "→") (map text xs)

gatherBinders :: Term -> M ([Doc], Doc)
gatherBinders (TmAbs b) =
    lunbind b $ \((n, _), body) -> do
        let dn = text $ name2String n
        (rest, bdy) <- gatherBinders body
        return (dn : rest, bdy)

gatherBinders body = do
    db <- display body
    return ([], db)
