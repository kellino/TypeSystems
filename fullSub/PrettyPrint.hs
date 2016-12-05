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
    display TmTrue = return $ text "true"
    display TmFalse = return $ text "false"

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

remComments :: T.Text -> T.Text
remComments = T.strip . T.takeWhile (/= '#') 
