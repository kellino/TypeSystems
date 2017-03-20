module Gamma where

import Syntax
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type Gamma = M.Map String GType

{-newtype Gamma = Gamma { gamma :: M.Map TName GType}-}

{-instance Show Gamma where-}
    {-show g = show (gamma g)-}

lookupType :: String -> Gamma -> GType
lookupType n g = fromMaybe (error "not found in env") (M.lookup n g)

-- for testing
env :: Gamma
env = M.empty
