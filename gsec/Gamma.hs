{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gamma where

import Syntax
import qualified Data.Map as M

newtype Gamma = Gamma (M.Map String GType) deriving (Show, Monoid)

lookupType :: String -> Gamma -> Maybe GType
lookupType n (Gamma g) = M.lookup n g

extend :: Gamma -> (String, GType) -> Gamma
extend (Gamma env) (nm, ty) = Gamma $ M.insert nm ty env

-- for testing
env :: Gamma
env = Gamma M.empty
