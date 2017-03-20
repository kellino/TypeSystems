module Repl where

import Syntax
import Gamma
import System.Console.Repline
import Control.Monad.State

import qualified Data.Map as M

data IState = IState 
            { typeEnv :: Gamma
            , termEnv :: TermEnv }

initState :: IState
initState = IState { ctx = env 
                   , termctx = undefined 
                   }


type Repl a = HaskelineT (StateT IState IO) a

evalDef :: 
