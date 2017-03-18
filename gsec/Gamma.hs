module Gamma where

import Syntax
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

newtype Gamma = Gamma { gamma :: M.Map TName GType}

lookupType :: TName -> Gamma -> GType
lookupType n g = fromMaybe (error "not found in env") (M.lookup n (gamma g)) 

-- for testing
env :: Gamma
env = Gamma { gamma = M.empty }
